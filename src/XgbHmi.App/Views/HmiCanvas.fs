/// 터치스크린(HMI) 작화 캔버스.
/// 편집 모드에서는 부품을 끌어 옮기고 크기를 조절하며, 끄면 실제 터치패널처럼 눌러서 PLC를 조작한다.
namespace XgbHmi.App.Views

open System
open System.Collections.Generic
open Avalonia
open Avalonia.Controls
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
open XgbHmi.Core
open XgbHmi.App.Themes
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

/// 터치패널 바탕. 패널 영역만 칠하고 편집 중에는 격자를 얹는다.
type internal PanelBackdrop(palette: Palette, step: float) =
    inherit Control()

    member val Palette = palette with get, set
    member val Step = step with get, set
    member val ShowGrid = true with get, set
    member val Background = "" with get, set
    member val PanelWidth = 1024.0 with get, set
    member val PanelHeight = 600.0 with get, set

    override this.Render(ctx: DrawingContext) =
        let p = this.Palette
        let pw = max 1.0 this.PanelWidth
        let ph = max 1.0 this.PanelHeight
        let page = Rect(0.0, 0.0, pw, ph)

        // 실제 터치패널처럼 어두운 바탕을 기본으로 둔다. 점등이 또렷하게 보인다.
        ctx.FillRectangle(Ui.brush (HmiParts.resolveBackground p this.Background), page)

        if this.ShowGrid then
            let pen = Pen(Ui.tint p.CanvasGrid 0.35, 1.0)
            let bold = Pen(Ui.tint p.CanvasGrid 0.7, 1.3)
            let mutable x = 0.0
            let mutable i = 0
            while x <= pw do
                ctx.DrawLine((if i % 5 = 0 then bold else pen), Point(x, 0.0), Point(x, ph))
                x <- x + this.Step
                i <- i + 1
            let mutable y = 0.0
            let mutable j = 0
            while y <= ph do
                ctx.DrawLine((if j % 5 = 0 then bold else pen), Point(0.0, y), Point(pw, y))
                y <- y + this.Step
                j <- j + 1

        // 패널 바깥 테두리 (실제 기기의 베젤 느낌)
        ctx.DrawRectangle(null, Pen(Ui.tint p.Accent 0.55, 1.5), page)


type internal PartResizeEdge =
    | PN
    | PS
    | PE
    | PW
    | PNE
    | PNW
    | PSE
    | PSW


/// 캔버스가 바깥(메인 창)에 요청하는 동작들
type HmiCanvasHost =
    { /// PLC 조작 통로. 운전 화면 카드와 같은 것을 쓴다.
      Cards: CardFactory.CardCallbacks
      /// 이동/크기조절 중 X/Y/W/H 실시간 표시
      Info: string -> unit
      /// 상호 배타 그룹: 넘긴 요소들을 한꺼번에 OFF 로 쓴다.
      ResetGroup: ElementVm list -> unit
      /// 비트 버튼 한 번 누름을 순서대로 실행한다.
      Press: HmiParts.PressPlan -> unit }


[<AllowNullLiteral>]
type HmiCanvasView(state: AppState, host: HmiCanvasHost) as this =

    let mutable palette = ThemeService.current ()
    let mutable editMode = false
    let mutable showGrid = true
    let mutable snapToGrid = true
    let mutable zoom = 1.0
    /// 창 크기에 맞춰 배율을 저절로 맞출지. (운전 중 띄우는 HMI 창)
    let mutable autoFit = false
    /// 처음 화면이 잡혔을 때 한 번만 배율을 맞춘다. (탭을 열자마자 패널 전체가 보이도록)
    let mutable fitPending = false

    let visuals = Dictionary<string, HmiParts.PartVisual>()
    let partCanvas = Canvas(Background = Brushes.Transparent)
    let overlay = Canvas(Background = null, IsHitTestVisible = false)
    let backdrop = PanelBackdrop(ThemeService.current (), 20.0)

    let contentGrid = Grid()
    let layoutTransform = LayoutTransformControl(Child = contentGrid)
    let scroller =
        ScrollViewer(
            Content = layoutTransform,
            HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto,
            VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto,
            Padding = Thickness 0.0,
            HorizontalContentAlignment = HorizontalAlignment.Center,
            VerticalContentAlignment = VerticalAlignment.Center
        )
    // 부품 좌표는 프로젝트 데이터라 RTL 언어에서도 캔버스는 항상 왼쪽->오른쪽으로 둔다.
    let rootBorder =
        Border(
            Background = Ui.brush (ThemeService.current ()).Window,
            Child = scroller,
            FlowDirection = FlowDirection.LeftToRight
        )

    // 드래그 / 크기조절 상태
    let mutable dragPart: HmiParts.PartVisual option = None
    let mutable dragStart = Point(0.0, 0.0)
    let mutable dragOrigin = (0, 0)
    let mutable resizeEdge: PartResizeEdge option = None
    let mutable resizeStart = Point(0.0, 0.0)
    let mutable resizeBounds = (0, 0, 0, 0)
    let handles = ResizeArray<Border>()
    let mutable pastePoint = (40, 40)

    /// 고른 부품을 감싸는 점선 테두리
    let selectionFrame =
        Border(
            BorderBrush = Ui.brush (ThemeService.current ()).Accent,
            BorderThickness = Thickness 2.0,
            Background = null,
            IsVisible = false,
            IsHitTestVisible = false
        )

    let subscriptions = ResizeArray<IDisposable>()
    let zoomChanged = Event<float>()
    let mutable lastStatus: CardFactory.RuntimeStatus option = None

    let snap (v: int) = if snapToGrid then int (Math.Round(float v / 10.0)) * 10 else v

    /// 부품 기본색의 기준이 되는 패널 바탕색
    let panelBackground () = HmiParts.resolveBackground palette state.HmiBackground

    let resolveElement (id: string) =
        if String.IsNullOrWhiteSpace id then None
        else state.Elements |> Seq.tryFind (fun e -> e.Id = id)

    let partHost: HmiParts.PartHost =
        { Cards = host.Cards
          Resolve = resolveElement
          Editing = fun () -> editMode
          GroupPeers =
            fun group selfId ->
                state.HmiParts
                |> Seq.filter (fun q ->
                    q.Id <> selfId
                    && not (String.IsNullOrWhiteSpace q.Group)
                    && String.Equals(q.Group, group, StringComparison.OrdinalIgnoreCase))
                |> Seq.choose (fun q -> resolveElement q.TargetId)
                // 같은 요소를 두 번 끄지 않는다.
                |> Seq.distinctBy (fun e -> e.Id)
                |> List.ofSeq
          ResetGroup = host.ResetGroup
          Press =
            fun target action part ->
                let peers =
                    if String.IsNullOrWhiteSpace part.Group then []
                    else
                        state.HmiParts
                        |> Seq.filter (fun q ->
                            q.Id <> part.Id
                            && not (String.IsNullOrWhiteSpace q.Group)
                            && String.Equals(q.Group, part.Group, StringComparison.OrdinalIgnoreCase))
                        |> Seq.choose (fun q -> resolveElement q.TargetId)
                        |> Seq.filter (fun e -> e.Id <> target.Id)
                        |> Seq.distinctBy (fun e -> e.Id)
                        |> List.ofSeq
                host.Press
                    { Target = target
                      Action = action
                      ResetOff = peers
                      ThenOn = resolveElement part.ThenOnId } }

    let applyPanelSize () =
        let w = float state.HmiWidth
        let h = float state.HmiHeight
        partCanvas.Width <- w
        partCanvas.Height <- h
        overlay.Width <- w
        overlay.Height <- h
        backdrop.Width <- w
        backdrop.Height <- h
        backdrop.PanelWidth <- w
        backdrop.PanelHeight <- h
        backdrop.Background <- state.HmiBackground
        backdrop.InvalidateVisual()

    let infoFor (vm: HmiPartVm) =
        host.Info(sprintf "%s   X=%d  Y=%d   W=%d  H=%d" (I18n.partLabel vm.Kind) vm.X vm.Y vm.Width vm.Height)

    let clearHandles () =
        for h in handles do
            overlay.Children.Remove h |> ignore
        handles.Clear()

    let visualOf (vm: HmiPartVm) =
        match visuals.TryGetValue vm.Id with
        | true, v -> Some v
        | _ -> None

    let positionFrame () =
        match state.HmiSelected with
        | Some vm when editMode ->
            Canvas.SetLeft(selectionFrame, float vm.X - 2.0)
            Canvas.SetTop(selectionFrame, float vm.Y - 2.0)
            selectionFrame.Width <- float vm.Width + 4.0
            selectionFrame.Height <- float vm.Height + 4.0
            selectionFrame.IsVisible <- true
        | _ -> selectionFrame.IsVisible <- false

    let positionHandles () =
        match state.HmiSelected with
        | Some vm when editMode && handles.Count = 8 ->
            let x = float vm.X
            let y = float vm.Y
            let w = float vm.Width
            let h = float vm.Height
            let half = 5.0
            let place (i: int) (px: float) (py: float) =
                Canvas.SetLeft(handles.[i], px - half)
                Canvas.SetTop(handles.[i], py - half)
            place 0 x y
            place 1 (x + w / 2.0) y
            place 2 (x + w) y
            place 3 (x + w) (y + h / 2.0)
            place 4 (x + w) (y + h)
            place 5 (x + w / 2.0) (y + h)
            place 6 x (y + h)
            place 7 x (y + h / 2.0)
        | _ -> ()

    let updateVisualBounds (vm: HmiPartVm) =
        match visualOf vm with
        | Some v ->
            Canvas.SetLeft(v.Root, float vm.X)
            Canvas.SetTop(v.Root, float vm.Y)
            v.Root.Width <- float vm.Width
            v.Root.Height <- float vm.Height
            v.Resize()
        | None -> ()

    let makeHandle (edge: PartResizeEdge) =
        let b =
            Border(
                Width = 10.0,
                Height = 10.0,
                Background = Ui.brush palette.Accent,
                BorderBrush = Ui.brush palette.Surface,
                BorderThickness = Thickness 1.5,
                CornerRadius = CornerRadius 2.0
            )
        b.Cursor <-
            match edge with
            | PN | PS -> new Cursor(StandardCursorType.SizeNorthSouth)
            | PE | PW -> new Cursor(StandardCursorType.SizeWestEast)
            | PNE | PSW -> new Cursor(StandardCursorType.TopRightCorner)
            | PNW | PSE -> new Cursor(StandardCursorType.TopLeftCorner)

        b.PointerPressed.Add(fun e ->
            if editMode then
                match state.HmiSelected with
                | Some vm ->
                    resizeEdge <- Some edge
                    resizeStart <- e.GetPosition overlay
                    resizeBounds <- (vm.X, vm.Y, vm.Width, vm.Height)
                    dragPart <- None
                    e.Pointer.Capture b
                    e.Handled <- true
                | None -> ())

        b.PointerMoved.Add(fun e ->
            match resizeEdge, state.HmiSelected with
            | Some edge, Some vm when editMode ->
                let now = e.GetPosition overlay
                let dx = int (now.X - resizeStart.X)
                let dy = int (now.Y - resizeStart.Y)
                let ox, oy, ow, oh = resizeBounds
                let mutable l = ox
                let mutable t = oy
                let mutable r = ox + ow
                let mutable bm = oy + oh
                (match edge with
                 | PW | PNW | PSW -> l <- l + dx
                 | _ -> ())
                (match edge with
                 | PE | PNE | PSE -> r <- r + dx
                 | _ -> ())
                (match edge with
                 | PN | PNW | PNE -> t <- t + dy
                 | _ -> ())
                (match edge with
                 | PS | PSW | PSE -> bm <- bm + dy
                 | _ -> ())

                if r - l < HmiLimits.minPartWidth then
                    match edge with
                    | PW | PNW | PSW -> l <- r - HmiLimits.minPartWidth
                    | _ -> r <- l + HmiLimits.minPartWidth
                if bm - t < HmiLimits.minPartHeight then
                    match edge with
                    | PN | PNW | PNE -> t <- bm - HmiLimits.minPartHeight
                    | _ -> bm <- t + HmiLimits.minPartHeight
                if l < 0 then
                    match edge with
                    | PW | PNW | PSW -> l <- 0
                    | _ -> ()
                if t < 0 then
                    match edge with
                    | PN | PNW | PNE -> t <- 0
                    | _ -> ()

                vm.SetBounds(snap l, snap t, snap (r - l), snap (bm - t))
                updateVisualBounds vm
                positionFrame ()
                positionHandles ()
                infoFor vm
                e.Handled <- true
            | _ -> ())

        b.PointerReleased.Add(fun e ->
            if resizeEdge.IsSome then
                resizeEdge <- None
                e.Pointer.Capture null
                positionFrame ()
                positionHandles ()
                e.Handled <- true)

        b

    let showHandles () =
        clearHandles ()
        if editMode then
            match state.HmiSelected with
            | Some _ ->
                for edge in [ PNW; PN; PNE; PE; PSE; PS; PSW; PW ] do
                    let h = makeHandle edge
                    handles.Add h
                    overlay.Children.Add h
                positionHandles ()
            | None -> ()

    let refreshSelectionVisual () =
        positionFrame ()
        showHandles ()
        match state.HmiSelected with
        | Some vm -> infoFor vm
        | None -> host.Info ""

    let partMenu (vm: HmiPartVm) =
        let menu = ContextMenu()
        menu.Items.Add(Ui.menuItem (I18n.t "cmd.copy") (fun () -> state.CopyPart vm)) |> ignore
        menu.Items.Add(Ui.menuItem (I18n.t "cmd.duplicate") (fun () -> state.DuplicatePart vm |> ignore)) |> ignore
        menu.Items.Add(Separator()) |> ignore
        menu.Items.Add(Ui.menuItem "▲" (fun () -> state.MovePartToFront vm)) |> ignore
        menu.Items.Add(Ui.menuItem "▼" (fun () -> state.MovePartToBack vm)) |> ignore
        menu.Items.Add(Separator()) |> ignore
        menu.Items.Add(Ui.menuItem (I18n.t "cmd.delete") (fun () -> state.RemovePart vm |> ignore)) |> ignore
        menu

    let canvasMenu () =
        let menu = ContextMenu()
        menu.Items.Add(
            Ui.menuItem (I18n.t "canvas.pasteHere") (fun () ->
                let x, y = pastePoint
                state.PastePartAt(x, y) |> ignore))
        |> ignore
        menu

    let findPartAt (source: obj) =
        let mutable result = None
        let mutable v = source :?> Visual
        while result.IsNone && not (isNull v) do
            match v with
            | :? Border as b when (b.Tag :? string) && visuals.ContainsKey(b.Tag :?> string) ->
                result <- Some visuals.[b.Tag :?> string]
            | _ -> ()
            v <- v.GetVisualParent()
        result

    let fitToWindow () =
        let vw = scroller.Viewport.Width
        let vh = scroller.Viewport.Height
        let pw = float state.HmiWidth
        let ph = float state.HmiHeight
        if vw > 40.0 && vh > 40.0 && pw > 0.0 && ph > 0.0 then
            let fit = min ((vw - 8.0) / pw) ((vh - 8.0) / ph)
            let z = max 0.15 (min 4.0 fit)
            if abs (z - zoom) > 0.001 then
                zoom <- z
                layoutTransform.LayoutTransform <- ScaleTransform(zoom, zoom)
                zoomChanged.Trigger zoom

    do
        contentGrid.Children.Add backdrop
        contentGrid.Children.Add partCanvas
        contentGrid.Children.Add overlay
        overlay.Children.Add selectionFrame
        partCanvas.ContextMenu <- canvasMenu ()

        // Ctrl(⌘) + 휠 = 배율
        scroller.AddHandler(
            InputElement.PointerWheelChangedEvent,
            (fun _ (e: PointerWheelEventArgs) ->
                if e.KeyModifiers.HasFlag KeyModifiers.Control || e.KeyModifiers.HasFlag KeyModifiers.Meta then
                    let step = if e.Delta.Y > 0.0 then 0.1 else -0.1
                    zoom <- max 0.15 (min 4.0 (zoom + step))
                    layoutTransform.LayoutTransform <- ScaleTransform(zoom, zoom)
                    zoomChanged.Trigger zoom
                    e.Handled <- true),
            RoutingStrategies.Tunnel
        )

        // 편집 중에는 부품 위의 버튼보다 먼저 이벤트를 가로챈다. (PLC 명령 잠금 + 드래그)
        partCanvas.AddHandler(
            InputElement.PointerPressedEvent,
            (fun _ (e: PointerPressedEventArgs) ->
                let point = e.GetCurrentPoint partCanvas
                let pos = point.Position
                if point.Properties.IsRightButtonPressed then
                    pastePoint <- (max 0 (int pos.X), max 0 (int pos.Y))
                    match findPartAt e.Source with
                    | Some v ->
                        partCanvas.ContextMenu <- partMenu v.Vm
                        if editMode then state.SelectPart(Some v.Vm)
                    | None -> partCanvas.ContextMenu <- canvasMenu ()
                elif editMode && point.Properties.IsLeftButtonPressed then
                    match findPartAt e.Source with
                    | Some v ->
                        state.SelectPart(Some v.Vm)
                        dragPart <- Some v
                        dragStart <- pos
                        dragOrigin <- (v.Vm.X, v.Vm.Y)
                        e.Pointer.Capture partCanvas
                        e.Handled <- true
                    | None ->
                        state.SelectPart None
                        e.Handled <- true),
            RoutingStrategies.Tunnel
        )

        partCanvas.PointerMoved.Add(fun e ->
            match dragPart with
            | Some v when editMode ->
                let pos = e.GetPosition partCanvas
                let ox, oy = dragOrigin
                v.Vm.X <- snap (max 0 (ox + int (pos.X - dragStart.X)))
                v.Vm.Y <- snap (max 0 (oy + int (pos.Y - dragStart.Y)))
                updateVisualBounds v.Vm
                positionFrame ()
                positionHandles ()
                infoFor v.Vm
            | _ -> ())

        partCanvas.PointerReleased.Add(fun e ->
            if dragPart.IsSome then
                dragPart <- None
                e.Pointer.Capture null
                positionFrame ()
                positionHandles ())

        scroller.PropertyChanged.Add(fun args ->
            if args.Property = ScrollViewer.ViewportProperty then
                if autoFit then fitToWindow ()
                elif fitPending && scroller.Viewport.Width > 40.0 && scroller.Viewport.Height > 40.0 then
                    fitPending <- false
                    fitToWindow ())

        subscriptions.Add(state.HmiSelectionChanged.Subscribe(fun () -> refreshSelectionVisual ()))

        subscriptions.Add(
            state.HmiScreenChanged.Subscribe(fun () ->
                applyPanelSize ()
                // 바탕색이 바뀌면 부품 기본색도 따라가야 한다.
                this.Rebuild()
                if autoFit then fitToWindow ()))

    interface IDisposable with
        member _.Dispose() =
            for s in subscriptions do
                s.Dispose()
            subscriptions.Clear()

    member _.Root: Control = rootBorder :> Control

    /// 부품 편집 모드. 끄면 실제 터치패널처럼 눌러서 조작한다.
    member _.EditMode
        with get () = editMode
        and set v =
            editMode <- v
            overlay.IsHitTestVisible <- v
            partCanvas.Cursor <- (if v then new Cursor(StandardCursorType.SizeAll) else Cursor.Default)
            if not v then
                dragPart <- None
                resizeEdge <- None
            backdrop.ShowGrid <- (v && showGrid)
            backdrop.InvalidateVisual()
            refreshSelectionVisual ()

    member _.ShowGrid
        with get () = showGrid
        and set v =
            showGrid <- v
            backdrop.ShowGrid <- (v && editMode)
            backdrop.InvalidateVisual()

    member _.SnapToGrid
        with get () = snapToGrid
        and set v = snapToGrid <- v

    /// 창 크기에 맞춰 배율을 저절로 맞출지 (운전 중 띄우는 HMI 창)
    member _.AutoFit
        with get () = autoFit
        and set v =
            autoFit <- v
            if v then fitToWindow ()

    member _.ZoomChanged = zoomChanged.Publish

    member _.Zoom
        with get () = zoom
        and set v =
            zoom <- max 0.15 (min 4.0 v)
            layoutTransform.LayoutTransform <- ScaleTransform(zoom, zoom)
            zoomChanged.Trigger zoom

    member _.FitToWindow() = fitToWindow ()

    /// 화면 크기가 잡히는 대로 한 번만 맞춘다. (탭을 처음 열 때)
    member _.FitWhenReady() =
        if scroller.Viewport.Width > 40.0 && scroller.Viewport.Height > 40.0 then fitToWindow ()
        else fitPending <- true

    /// 팔레트가 바뀌면 부품 전체를 다시 만든다.
    member this.ApplyPalette(p: Palette) =
        palette <- p
        backdrop.Palette <- p
        rootBorder.Background <- Ui.brush p.Window
        selectionFrame.BorderBrush <- Ui.brush p.Accent
        this.Rebuild()

    /// 부품 목록/종류가 바뀌었을 때 전부 새로 만든다.
    member _.Rebuild() =
        partCanvas.Children.Clear()
        clearHandles ()
        visuals.Clear()
        overlay.Children.Clear()
        overlay.Children.Add selectionFrame
        backdrop.Palette <- palette
        backdrop.ShowGrid <- (showGrid && editMode)
        for vm in state.HmiParts do
            let v = HmiParts.create palette (panelBackground ()) vm partHost
            Canvas.SetLeft(v.Root, float vm.X)
            Canvas.SetTop(v.Root, float vm.Y)
            partCanvas.Children.Add v.Root
            visuals.[vm.Id] <- v
        applyPanelSize ()
        refreshSelectionVisual ()
        match lastStatus with
        | Some s -> for kv in visuals do kv.Value.Refresh s
        | None -> ()

    /// 부품 하나만 다시 만든다 (글자/색을 고칠 때 전체를 새로 만들지 않도록)
    member _.RebuildOne(vm: HmiPartVm) =
        match visuals.TryGetValue vm.Id with
        | true, old ->
            partCanvas.Children.Remove old.Root |> ignore
            visuals.Remove vm.Id |> ignore
        | _ -> ()
        // 겹침 순서를 지키려고 제자리에 다시 꽂는다.
        let index = state.HmiParts.IndexOf vm
        let v = HmiParts.create palette (panelBackground ()) vm partHost
        Canvas.SetLeft(v.Root, float vm.X)
        Canvas.SetTop(v.Root, float vm.Y)
        if index >= 0 && index < partCanvas.Children.Count then
            partCanvas.Children.Insert(index, v.Root)
        else
            partCanvas.Children.Add v.Root
        visuals.[vm.Id] <- v
        refreshSelectionVisual ()
        match lastStatus with
        | Some s -> v.Refresh s
        | None -> ()

    /// 부품 위치/크기만 바뀌었을 때
    member _.UpdateBounds(vm: HmiPartVm) =
        updateVisualBounds vm
        positionFrame ()
        positionHandles ()

    /// PLC 값 갱신
    member _.RefreshValues(status: CardFactory.RuntimeStatus) =
        lastStatus <- Some status
        for kv in visuals do
            kv.Value.Refresh status
