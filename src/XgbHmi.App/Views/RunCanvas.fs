/// 운전 화면 캔버스. 배치 편집(드래그 이동 / 8방향 크기조절 / 우클릭 메뉴)을 담당한다.
namespace XgbHmi.App.Views

open System
open System.Collections.Generic
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Interactivity
open Avalonia.Layout
open Avalonia.Media
open Avalonia.VisualTree
open XgbHmi.Core
open XgbHmi.App.Themes
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

type internal ResizeEdge =
    | N
    | S
    | E
    | W
    | NE
    | NW
    | SE
    | SW

/// 캔버스가 바깥(메인 창)에 요청하는 동작들
type CanvasHost =
    { Cards: CardFactory.CardCallbacks
      CopyItem: ElementVm -> unit
      DuplicateItem: ElementVm -> unit
      DeleteItem: ElementVm -> unit
      PasteAt: int -> int -> unit
      /// 이동/크기조절 중 X/Y/W/H 실시간 표시
      Info: string -> unit }

/// 격자 배경 + 도면(페이지) 영역 (XG5000 / GT Designer 의 작화 영역 느낌)
type internal GridBackdrop(palette: Palette, step: float) =
    inherit Control()

    member val Palette = palette with get, set
    member val Step = step with get, set
    member val Visible = true with get, set
    /// 배치할 수 있는 도면 크기. 이 영역만 밝게 칠하고 격자를 그린다.
    member val PageWidth = 1600.0 with get, set
    member val PageHeight = 1000.0 with get, set

    override this.Render(ctx: DrawingContext) =
        let p = this.Palette
        let pw = max 1.0 this.PageWidth
        let ph = max 1.0 this.PageHeight
        let page = Rect(0.0, 0.0, pw, ph)

        // 도면 영역: 캔버스 바깥(스크롤 여유 공간)과 구분되도록 살짝 밝게 칠한다.
        ctx.FillRectangle(Ui.tint p.Surface (if p.IsDark then 0.35 else 0.8), page)

        if this.Visible then
            let pen = Pen(Ui.brush p.CanvasGrid, 1.0)
            let bold = Pen(Ui.tint p.CanvasGrid 0.9, 1.4)
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

        // 도면 경계선
        ctx.DrawRectangle(null, Pen(Ui.tint p.Accent 0.55, 1.5), page)

[<AllowNullLiteral>]
type RunCanvasView(state: AppState, host: CanvasHost) =

    let mutable palette = ThemeService.current ()
    let mutable layoutMode = false
    let mutable showGrid = true
    let mutable snapToGrid = false
    let mutable zoom = 1.0

    let cards = Dictionary<string, CardFactory.RuntimeCard>()
    let cardCanvas = Canvas(Background = Brushes.Transparent)
    let overlay = Canvas(Background = null, IsHitTestVisible = false)
    let backdrop = GridBackdrop(ThemeService.current (), 20.0)

    let contentGrid = Grid()
    let layoutTransform = LayoutTransformControl(Child = contentGrid)
    let scroller =
        ScrollViewer(
            Content = layoutTransform,
            HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto,
            VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto,
            Padding = Thickness 0.0
        )
    // 도면 좌표(X/Y)는 프로젝트 데이터이므로 아랍어 같은 RTL 언어에서도 캔버스는 항상 왼쪽->오른쪽으로 둔다.
    let rootBorder =
        Border(
            Background = Ui.brush (ThemeService.current ()).CanvasBg,
            Child = scroller,
            FlowDirection = FlowDirection.LeftToRight
        )

    // 드래그 / 크기조절 상태
    let mutable dragCard: CardFactory.RuntimeCard option = None
    let mutable dragStart = Point(0.0, 0.0)
    let mutable dragOrigin = (0, 0)
    let mutable resizeEdge: ResizeEdge option = None
    let mutable resizeStart = Point(0.0, 0.0)
    let mutable resizeBounds = (0, 0, 0, 0)
    let handles = ResizeArray<Border>()
    let mutable pastePoint = (40, 40)
    let mutable screenWidth = float Limits.defaultScreenWidth
    let mutable screenHeight = float Limits.defaultScreenHeight
    /// 영역 선택(고무줄)
    let mutable bandStart: Point option = None
    let bandRect =
        Border(
            Background = Ui.tint (ThemeService.current ()).Accent 0.15,
            BorderBrush = Ui.brush (ThemeService.current ()).Accent,
            BorderThickness = Thickness 1.0,
            IsVisible = false,
            IsHitTestVisible = false
        )
    /// 화면을 다시 만들 때 이전 구독을 끊기 위해 모아 둔다.
    let subscriptions = ResizeArray<IDisposable>()
    let zoomChanged = Event<float>()

    let snap (v: int) = if snapToGrid then int (Math.Round(float v / 10.0)) * 10 else v

    let contentSize () =
        // 도면 크기가 기본이고, 요소가 밖으로 나가 있으면 그만큼 더 넓힌다. (스크롤 영역)
        let mutable maxX = int screenWidth
        let mutable maxY = int screenHeight
        for e in state.Elements do
            maxX <- max maxX (e.X + e.Width + 40)
            maxY <- max maxY (e.Y + e.Height + 40)
        float maxX, float maxY

    let applyContentSize () =
        let w, h = contentSize ()
        cardCanvas.Width <- w
        cardCanvas.Height <- h
        overlay.Width <- w
        overlay.Height <- h
        backdrop.Width <- w
        backdrop.Height <- h
        backdrop.PageWidth <- screenWidth
        backdrop.PageHeight <- screenHeight
        backdrop.InvalidateVisual()

    let infoFor (vm: ElementVm) =
        host.Info(sprintf "%s   X=%d  Y=%d   W=%d  H=%d" vm.Name vm.X vm.Y vm.Width vm.Height)

    let clearHandles () =
        for h in handles do
            overlay.Children.Remove h |> ignore
        handles.Clear()

    let cardOf (vm: ElementVm) =
        match cards.TryGetValue vm.Id with
        | true, c -> Some c
        | _ -> None

    let positionHandles () =
        match state.Primary with
        | Some vm when layoutMode && handles.Count = 8 ->
            let x = float vm.X
            let y = float vm.Y
            let w = float vm.Width
            let h = float vm.Height
            let size = 10.0
            let half = size / 2.0
            let place (i: int) (px: float) (py: float) =
                Canvas.SetLeft(handles.[i], px - half)
                Canvas.SetTop(handles.[i], py - half)
            place 0 x y                       // NW
            place 1 (x + w / 2.0) y           // N
            place 2 (x + w) y                 // NE
            place 3 (x + w) (y + h / 2.0)     // E
            place 4 (x + w) (y + h)           // SE
            place 5 (x + w / 2.0) (y + h)     // S
            place 6 x (y + h)                 // SW
            place 7 x (y + h / 2.0)           // W
        | _ -> ()

    let updateCardVisual (vm: ElementVm) =
        match cardOf vm with
        | Some card ->
            Canvas.SetLeft(card.Root, float vm.X)
            Canvas.SetTop(card.Root, float vm.Y)
            card.Root.Width <- float vm.Width
            card.Root.Height <- float vm.Height
        | None -> ()

    // ---- 크기조절 핸들 ----
    let makeHandle (edge: ResizeEdge) =
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
            | N | S -> new Cursor(StandardCursorType.SizeNorthSouth)
            | E | W -> new Cursor(StandardCursorType.SizeWestEast)
            | NE | SW -> new Cursor(StandardCursorType.TopRightCorner)
            | NW | SE -> new Cursor(StandardCursorType.TopLeftCorner)

        b.PointerPressed.Add(fun e ->
            if layoutMode then
                match state.Primary with
                | Some vm ->
                    resizeEdge <- Some edge
                    resizeStart <- e.GetPosition overlay
                    resizeBounds <- (vm.X, vm.Y, vm.Width, vm.Height)
                    dragCard <- None
                    e.Pointer.Capture b
                    e.Handled <- true
                | None -> ())

        b.PointerMoved.Add(fun e ->
            match resizeEdge, state.Primary with
            | Some edge, Some vm when layoutMode ->
                let now = e.GetPosition overlay
                let dx = int (now.X - resizeStart.X)
                let dy = int (now.Y - resizeStart.Y)
                let ox, oy, ow, oh = resizeBounds
                let mutable l = ox
                let mutable t = oy
                let mutable r = ox + ow
                let mutable bm = oy + oh
                (match edge with
                 | W | NW | SW -> l <- l + dx
                 | _ -> ())
                (match edge with
                 | E | NE | SE -> r <- r + dx
                 | _ -> ())
                (match edge with
                 | N | NW | NE -> t <- t + dy
                 | _ -> ())
                (match edge with
                 | S | SW | SE -> bm <- bm + dy
                 | _ -> ())

                if r - l < Limits.minWidth then
                    match edge with
                    | W | NW | SW -> l <- r - Limits.minWidth
                    | _ -> r <- l + Limits.minWidth
                if bm - t < Limits.minHeight then
                    match edge with
                    | N | NW | NE -> t <- bm - Limits.minHeight
                    | _ -> bm <- t + Limits.minHeight
                if l < 0 then
                    match edge with
                    | W | NW | SW -> l <- 0
                    | _ -> ()
                if t < 0 then
                    match edge with
                    | N | NW | NE -> t <- 0
                    | _ -> ()

                vm.SetBounds(snap l, snap t, snap (r - l), snap (bm - t))
                updateCardVisual vm
                positionHandles ()
                infoFor vm
                e.Handled <- true
            | _ -> ())

        b.PointerReleased.Add(fun e ->
            if resizeEdge.IsSome then
                resizeEdge <- None
                e.Pointer.Capture null
                applyContentSize ()
                positionHandles ()
                e.Handled <- true)

        b

    let showHandles () =
        clearHandles ()
        if layoutMode then
            match state.Primary with
            | Some _ ->
                for edge in [ NW; N; NE; E; SE; S; SW; W ] do
                    let h = makeHandle edge
                    handles.Add h
                    overlay.Children.Add h
                positionHandles ()
            | None -> ()

    let refreshSelectionVisual () =
        for kv in cards do
            let card = kv.Value
            let selected = state.IsSelected card.Vm
            if selected && layoutMode then
                card.Root.BorderBrush <- Ui.brush palette.Accent
                card.Root.BorderThickness <- Thickness 2.0
            elif selected then
                card.Root.BorderBrush <- Ui.brush palette.Accent
                card.Root.BorderThickness <- Thickness 1.5
            else
                card.Root.BorderBrush <- Ui.brush palette.CardBorder
                card.Root.BorderThickness <- Thickness 1.0
        showHandles ()
        match state.Primary with
        | Some vm -> infoFor vm
        | None -> host.Info ""

    /// 요소 우클릭 메뉴 (복사 / 복제 / 삭제)
    let cardMenu (vm: ElementVm) =
        let menu = ContextMenu()
        menu.Items.Add(Ui.menuItem (I18n.t "cmd.copy") (fun () -> host.CopyItem vm)) |> ignore
        menu.Items.Add(Ui.menuItem (I18n.t "cmd.duplicate") (fun () -> host.DuplicateItem vm)) |> ignore
        menu.Items.Add(Separator()) |> ignore
        menu.Items.Add(Ui.menuItem (I18n.t "cmd.delete") (fun () -> host.DeleteItem vm)) |> ignore
        menu

    let canvasMenu () =
        let menu = ContextMenu()
        menu.Items.Add(
            Ui.menuItem (I18n.t "canvas.pasteHere") (fun () ->
                let x, y = pastePoint
                host.PasteAt x y))
        |> ignore
        menu

    let findCardAt (source: obj) =
        let mutable result = None
        let mutable v = source :?> Visual
        while result.IsNone && not (isNull v) do
            match v with
            | :? Border as b when (b.Tag :? string) && cards.ContainsKey(b.Tag :?> string) ->
                result <- Some cards.[b.Tag :?> string]
            | _ -> ()
            v <- v.GetVisualParent()
        result

    /// 끄는 동안 화면 가장자리에 닿으면 저절로 스크롤한다.
    let autoScroll (pointerInCanvas: Point) =
        let viewport = scroller.Viewport
        let offset = scroller.Offset
        let margin = 48.0
        let step = 26.0
        let px = pointerInCanvas.X * zoom - offset.X
        let py = pointerInCanvas.Y * zoom - offset.Y
        let mutable dx = 0.0
        let mutable dy = 0.0
        if px < margin then dx <- -step
        elif px > viewport.Width - margin then dx <- step
        if py < margin then dy <- -step
        elif py > viewport.Height - margin then dy <- step
        if dx <> 0.0 || dy <> 0.0 then
            let maxX = max 0.0 (scroller.Extent.Width - viewport.Width)
            let maxY = max 0.0 (scroller.Extent.Height - viewport.Height)
            scroller.Offset <- Vector(min maxX (max 0.0 (offset.X + dx)), min maxY (max 0.0 (offset.Y + dy)))

    /// 고무줄 선택 사각형을 갱신한다.
    let updateBand (origin: Point) (now: Point) =
        let x = min origin.X now.X
        let y = min origin.Y now.Y
        Canvas.SetLeft(bandRect, x)
        Canvas.SetTop(bandRect, y)
        bandRect.Width <- abs (now.X - origin.X)
        bandRect.Height <- abs (now.Y - origin.Y)
        bandRect.IsVisible <- true

    let finishBand (origin: Point) (now: Point) =
        bandRect.IsVisible <- false
        let x1 = min origin.X now.X
        let y1 = min origin.Y now.Y
        let x2 = max origin.X now.X
        let y2 = max origin.Y now.Y
        if x2 - x1 > 4.0 && y2 - y1 > 4.0 then
            let hit =
                state.Elements
                |> Seq.filter (fun e ->
                    e.Enabled
                    && e.Visible
                    && float e.X < x2
                    && x1 < float (e.X + e.Width)
                    && float e.Y < y2
                    && y1 < float (e.Y + e.Height))
                |> List.ofSeq
            state.SelectMany hit

    do
        contentGrid.Children.Add backdrop
        contentGrid.Children.Add cardCanvas
        contentGrid.Children.Add overlay
        overlay.Children.Add bandRect
        cardCanvas.ContextMenu <- canvasMenu ()

        // Ctrl(⌘) + 휠 = 배율, Shift + 휠 = 가로 스크롤
        scroller.AddHandler(
            InputElement.PointerWheelChangedEvent,
            (fun _ (e: PointerWheelEventArgs) ->
                if e.KeyModifiers.HasFlag KeyModifiers.Control || e.KeyModifiers.HasFlag KeyModifiers.Meta then
                    let step = if e.Delta.Y > 0.0 then 0.1 else -0.1
                    zoom <- max 0.4 (min 3.0 (zoom + step))
                    layoutTransform.LayoutTransform <- ScaleTransform(zoom, zoom)
                    zoomChanged.Trigger zoom
                    e.Handled <- true
                elif e.KeyModifiers.HasFlag KeyModifiers.Shift then
                    let offset = scroller.Offset
                    let maxX = max 0.0 (scroller.Extent.Width - scroller.Viewport.Width)
                    scroller.Offset <- Vector(min maxX (max 0.0 (offset.X - e.Delta.Y * 60.0)), offset.Y)
                    e.Handled <- true),
            RoutingStrategies.Tunnel
        )

        // 배치 편집에서는 카드 위의 버튼보다 먼저 이벤트를 가로챈다. (PLC 명령 잠금 + 드래그)
        cardCanvas.AddHandler(
            InputElement.PointerPressedEvent,
            (fun _ (e: PointerPressedEventArgs) ->
                let point = e.GetCurrentPoint cardCanvas
                let pos = point.Position
                if point.Properties.IsRightButtonPressed then
                    pastePoint <- (max 0 (int pos.X), max 0 (int pos.Y))
                    match findCardAt e.Source with
                    | Some card ->
                        cardCanvas.ContextMenu <- cardMenu card.Vm
                        if layoutMode then state.Select(Some card.Vm, false)
                    | None -> cardCanvas.ContextMenu <- canvasMenu ()
                elif layoutMode && point.Properties.IsLeftButtonPressed then
                    match findCardAt e.Source with
                    | Some card ->
                        let additive =
                            e.KeyModifiers.HasFlag KeyModifiers.Control
                            || e.KeyModifiers.HasFlag KeyModifiers.Meta
                        state.Select(Some card.Vm, additive)
                        dragCard <- Some card
                        dragStart <- pos
                        dragOrigin <- (card.Vm.X, card.Vm.Y)
                        e.Pointer.Capture cardCanvas
                        e.Handled <- true
                    | None ->
                        state.ClearSelection()
                        bandStart <- Some pos
                        updateBand pos pos
                        bandRect.IsVisible <- false
                        e.Pointer.Capture cardCanvas
                        e.Handled <- true),
            RoutingStrategies.Tunnel
        )

        cardCanvas.PointerMoved.Add(fun e ->
            let pos = e.GetPosition cardCanvas
            match dragCard, bandStart with
            | Some card, _ when layoutMode ->
                let ox, oy = dragOrigin
                let nx = max 0 (ox + int (pos.X - dragStart.X))
                let ny = max 0 (oy + int (pos.Y - dragStart.Y))
                card.Vm.X <- snap nx
                card.Vm.Y <- snap ny
                updateCardVisual card.Vm
                positionHandles ()
                infoFor card.Vm
                autoScroll pos
            | _, Some origin when layoutMode ->
                updateBand origin pos
                autoScroll pos
            | _ -> ())

        cardCanvas.PointerReleased.Add(fun e ->
            let pos = e.GetPosition cardCanvas
            if dragCard.IsSome then
                dragCard <- None
                e.Pointer.Capture null
                state.GrowScreenToFit() |> ignore
                applyContentSize ()
                positionHandles ()
            match bandStart with
            | Some origin ->
                bandStart <- None
                e.Pointer.Capture null
                finishBand origin pos
            | None -> ())

        subscriptions.Add(state.SelectionChanged.Subscribe(fun () -> refreshSelectionVisual ()))

        subscriptions.Add(
            state.ItemChanged.Subscribe(fun (vm, prop) ->
                match prop with
                | "X" | "Y" | "Width" | "Height" ->
                    updateCardVisual vm
                    if state.IsSelected vm then positionHandles ()
                    applyContentSize ()
                | _ -> ()))

    interface IDisposable with
        member _.Dispose() =
            for s in subscriptions do
                s.Dispose()
            subscriptions.Clear()

    member _.Root: Control = rootBorder :> Control

    /// 현재 보이는 캔버스 가로 폭 (자동 배치의 줄바꿈 기준)
    member _.ViewportWidth =
        let w = scroller.Viewport.Width
        if w > 100.0 then w / (max 0.1 zoom) else float Limits.minWidth * 8.0

    member _.LayoutMode
        with get () = layoutMode
        and set v =
            layoutMode <- v
            overlay.IsHitTestVisible <- v
            cardCanvas.Cursor <- (if v then new Cursor(StandardCursorType.SizeAll) else Cursor.Default)
            if not v then
                dragCard <- None
                resizeEdge <- None
            refreshSelectionVisual ()

    member _.ShowGrid
        with get () = showGrid
        and set v =
            showGrid <- v
            backdrop.Visible <- v
            backdrop.InvalidateVisual()

    member _.SnapToGrid
        with get () = snapToGrid
        and set v = snapToGrid <- v

    /// 배율이 바뀌면 알려 준다 (툴바 표시 갱신용)
    member _.ZoomChanged = zoomChanged.Publish

    member _.Zoom
        with get () = zoom
        and set v =
            zoom <- max 0.4 (min 3.0 v)
            layoutTransform.LayoutTransform <- ScaleTransform(zoom, zoom)
            zoomChanged.Trigger zoom

    /// 도면 전체가 창에 들어오도록 배율을 맞춘다.
    member this.FitToWindow() =
        let w, h = contentSize ()
        let viewport = scroller.Viewport
        if viewport.Width > 50.0 && viewport.Height > 50.0 && w > 0.0 && h > 0.0 then
            let fit = min ((viewport.Width - 8.0) / w) ((viewport.Height - 8.0) / h)
            this.Zoom <- max 0.4 (min 1.0 fit)
            scroller.Offset <- Vector(0.0, 0.0)

    /// 보이는 요소들이 창을 꽉 채우도록 배율을 맞춘다. 요소가 몇 개 없으면 크게 확대한다.
    /// (운전 화면 모니터링 창처럼 멀리서 보는 화면용. 도면 전체가 아니라 실제 내용에 맞춘다)
    member this.FitToContent() =
        let shown = state.Elements |> Seq.filter (fun e -> e.Enabled && e.Visible) |> List.ofSeq
        match shown with
        | [] -> this.FitToWindow()
        | _ ->
            let left = shown |> List.map (fun e -> e.X) |> List.min
            let top = shown |> List.map (fun e -> e.Y) |> List.min
            let right = shown |> List.map (fun e -> e.X + e.Width) |> List.max
            let bottom = shown |> List.map (fun e -> e.Y + e.Height) |> List.max
            let w = float (right - left) + 32.0
            let h = float (bottom - top) + 32.0
            let viewport = scroller.Viewport
            if viewport.Width > 50.0 && viewport.Height > 50.0 && w > 0.0 && h > 0.0 then
                let fit = min (viewport.Width / w) (viewport.Height / h)
                this.Zoom <- max 0.4 (min 3.0 fit)
                // 내용 왼쪽 위가 보이도록 스크롤을 옮긴다.
                let z = this.Zoom
                scroller.Offset <- Vector(max 0.0 (float left * z - 16.0), max 0.0 (float top * z - 16.0))

    /// 도면 크기를 바꾼다.
    member _.SetScreenSize(width: int, height: int) =
        screenWidth <- float width
        screenHeight <- float height
        applyContentSize ()

    /// 팔레트가 바뀌면 카드 전체를 다시 만든다.
    member this.ApplyPalette(p: Palette) =
        palette <- p
        rootBorder.Background <- Ui.brush p.CanvasBg
        this.Rebuild()

    /// 요소 목록/종류가 바뀌었을 때 카드를 새로 만든다.
    member _.Rebuild() =
        cardCanvas.Children.Clear()
        clearHandles ()
        cards.Clear()
        overlay.Children.Clear()
        overlay.Children.Add bandRect
        backdrop.Palette <- palette
        backdrop.Visible <- showGrid
        screenWidth <- float state.ScreenWidth
        screenHeight <- float state.ScreenHeight
        for e in state.Elements do
            if e.Enabled && e.Visible then
                let card = CardFactory.create palette e host.Cards
                Canvas.SetLeft(card.Root, float e.X)
                Canvas.SetTop(card.Root, float e.Y)
                cardCanvas.Children.Add card.Root
                cards.[e.Id] <- card
        applyContentSize ()
        refreshSelectionVisual ()

    /// 요소 하나만 다시 만든다 (이름/주소를 고칠 때 전체를 새로 만들지 않도록)
    member _.RebuildOne(vm: ElementVm) =
        match cards.TryGetValue vm.Id with
        | true, old ->
            cardCanvas.Children.Remove old.Root |> ignore
            cards.Remove vm.Id |> ignore
        | _ -> ()
        if vm.Enabled && vm.Visible then
            let card = CardFactory.create palette vm host.Cards
            Canvas.SetLeft(card.Root, float vm.X)
            Canvas.SetTop(card.Root, float vm.Y)
            cardCanvas.Children.Add card.Root
            cards.[vm.Id] <- card
        applyContentSize ()
        refreshSelectionVisual ()

    /// PLC 값 갱신
    member _.RefreshValues(status: CardFactory.RuntimeStatus) =
        for kv in cards do
            kv.Value.Refresh status

    /// 화면 가운데로 특정 요소를 보이게 한다 (트리에서 선택했을 때)
    member _.BringIntoView(vm: ElementVm) =
        match cards.TryGetValue vm.Id with
        | true, card -> card.Root.BringIntoView()
        | _ -> ()
