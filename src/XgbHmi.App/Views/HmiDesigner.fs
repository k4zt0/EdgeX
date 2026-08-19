/// HMI 탭 전체. 왼쪽부터 툴바 / 터치스크린 작화 캔버스 / 부품 속성.
namespace XgbHmi.App.Views

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.App.Themes
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

[<AllowNullLiteral>]
type HmiDesignerView(state: AppState, host: HmiCanvasHost, notify: string -> unit) =

    let p = ThemeService.current ()
    let canvas = new HmiCanvasView(state, host)

    // 배치를 잡는 일이 먼저라 부품 편집을 켠 채로 시작한다.
    // 툴바를 만들기 전에 정해야 토글이 실제 상태와 같게 그려진다.
    do canvas.EditMode <- true

    let subscriptions = ResizeArray<IDisposable>()
    /// 속성창을 만들면서 넣는 초깃값이 '사용자가 고친 값' 으로 잡히지 않게 막는다.
    let mutable suppress = false

    let inspectorBody = StackPanel(Orientation = Orientation.Vertical, Spacing = 2.0, Margin = Thickness(10.0, 8.0, 10.0, 12.0))

    let hintText = Ui.muted ""
    let hintBar = Border(Child = hintText)

    let mutable zoomLabel: TextBlock = null
    let mutable widthBox: NumericUpDown = null
    let mutable heightBox: NumericUpDown = null

    // 위치/크기 칸은 캔버스에서 끌 때도 따라 움직여야 해서 따로 들고 있는다.
    let mutable xBox: NumericUpDown = null
    let mutable yBox: NumericUpDown = null
    let mutable wBox: NumericUpDown = null
    let mutable hBox: NumericUpDown = null

    /// 색 고르기에 쓰는 기본 색. 첫 항목은 '테마 기본'(빈 문자열)이다.
    let swatches =
        [ ""
          "#E23B4E"
          "#F0A500"
          "#2FA84F"
          "#2D7FF9"
          "#7A5CE0"
          "#E0457B"
          "#00B3B3"
          "#F2F4F8"
          "#9AA4B2"
          "#1E232B" ]

    let setHint () =
        let on = canvas.EditMode
        hintText.Text <- (if on then I18n.t "hmi.hint" else I18n.t "hmi.hintOff")
        hintText.Foreground <- Ui.brush (if on then p.Accent else p.TextMuted)
        hintBar.Background <- (if on then Ui.tint p.Accent 0.14 else Ui.tint p.Off 0.10)

    // ---------------------------------------------------------------- 속성칸
    let labelled (caption: string) (editor: Control) = Ui.field caption editor :> Control

    let sectionGap () =
        let b = Border(Height = 8.0)
        b :> Control

    let colorBox (current: string) (onPick: string -> unit) =
        let combo = ComboBox(HorizontalAlignment = HorizontalAlignment.Stretch)
        for hex in swatches do
            let row = StackPanel(Orientation = Orientation.Horizontal, Spacing = 8.0)
            let chip =
                Border(
                    Width = 16.0,
                    Height = 16.0,
                    CornerRadius = CornerRadius 3.0,
                    BorderBrush = Ui.brush p.Border,
                    BorderThickness = Thickness 1.0,
                    Background = (if hex = "" then Ui.tint p.Off 0.25 else Ui.brush hex),
                    VerticalAlignment = VerticalAlignment.Center
                )
            let caption = Ui.text (if hex = "" then I18n.t "hmi.themeDefault" else hex)
            caption.FontSize <- 11.5
            row.Children.Add chip
            row.Children.Add caption
            combo.Items.Add row |> ignore
        let index = swatches |> List.tryFindIndex (fun h -> String.Equals(h, current, StringComparison.OrdinalIgnoreCase))
        combo.SelectedIndex <- defaultArg index 0
        combo.SelectionChanged.Add(fun _ ->
            if not suppress && combo.SelectedIndex >= 0 then onPick swatches.[combo.SelectedIndex])
        combo :> Control

    let textBox (current: string) (onEdit: string -> unit) =
        let box = TextBox(Text = current)
        box.TextChanged.Add(fun _ -> if not suppress then onEdit box.Text)
        box :> Control

    let numberBox (min: int) (max: int) (current: int) (onEdit: int -> unit) =
        let box =
            NumericUpDown(
                Minimum = decimal min,
                Maximum = decimal max,
                Value = decimal current,
                Increment = 1m,
                FormatString = "0"
            )
        box.ValueChanged.Add(fun _ ->
            if not suppress && box.Value.HasValue then onEdit (int box.Value.Value))
        box

    let checkBox (caption: string) (current: bool) (onEdit: bool -> unit) =
        let box = CheckBox(Content = caption, IsChecked = current, FontFamily = Ui.uiFont)
        box.IsCheckedChanged.Add(fun _ ->
            if not suppress then onEdit (box.IsChecked.HasValue && box.IsChecked.Value))
        box :> Control

    let comboBox (items: (string * string) list) (current: string) (onPick: string -> unit) =
        // (값, 보여 줄 글자) 짝
        let combo = ComboBox(HorizontalAlignment = HorizontalAlignment.Stretch)
        for _, caption in items do
            combo.Items.Add(ComboBoxItem(Content = caption, FontFamily = Ui.uiFont)) |> ignore
        let index = items |> List.tryFindIndex (fun (v, _) -> v = current)
        combo.SelectedIndex <- defaultArg index 0
        combo.SelectionChanged.Add(fun _ ->
            if not suppress && combo.SelectedIndex >= 0 then onPick (fst items.[combo.SelectedIndex]))
        combo :> Control

    /// 부품 종류에 맞는 연결 후보만 고른다. 비트 부품에 D 주소를 물리면 설비가 잘못 돈다.
    let candidatesFor (kind: HmiPartKind) =
        state.Elements
        |> Seq.filter (fun e ->
            e.Enabled
            && not (String.IsNullOrWhiteSpace e.Device)
            && (match kind with
                | PartButton
                | PartToggle -> ItemKind.hasAction e.Kind
                | PartLamp
                | PartLampArray -> ItemKind.isBit e.Kind
                | kind when HmiPartKind.isWordPart kind -> ItemKind.isWord e.Kind
                | _ -> false))
        |> List.ofSeq

    let targetCombo (kind: HmiPartKind) (current: string) (onPick: string -> unit) =
        let items =
            ("", I18n.t "hmi.none")
            :: (candidatesFor kind
                |> List.map (fun e ->
                    let name = if String.IsNullOrWhiteSpace e.Name then "(no name)" else e.Name
                    e.Id, sprintf "%s  ·  %s" name e.Device))
        comboBox items current onPick

    let rebuildInspector () =
        suppress <- true
        inspectorBody.Children.Clear()
        xBox <- null
        yBox <- null
        wBox <- null
        hBox <- null

        match state.HmiSelected with
        | None ->
            // 아직 아무것도 놓지 않았으면 무엇부터 하면 되는지 알려 준다.
            let message =
                if state.HmiParts.Count = 0 then I18n.t "hmi.noParts" else I18n.t "hmi.selectPart"
            let empty = Ui.muted message
            empty.TextWrapping <- TextWrapping.Wrap
            empty.FontSize <- 12.0
            inspectorBody.Children.Add empty
        | Some vm ->
            let add (c: Control) = inspectorBody.Children.Add c

            let kindTag = Ui.title 12.5 (I18n.partLabel vm.Kind)
            kindTag.Foreground <- Ui.brush p.Accent
            add kindTag
            add (sectionGap ())

            if HmiPartKind.needsTarget vm.Kind then
                add (labelled (I18n.t "hmi.target") (targetCombo vm.Kind vm.TargetId (fun id -> vm.TargetId <- id)))
                if vm.Kind = PartValue then
                    add (labelled (I18n.t "hmi.subTarget") (targetCombo vm.Kind vm.SubTargetId (fun id -> vm.SubTargetId <- id)))

            let textLabel = if vm.Kind = PartClock then I18n.t "hmi.clockFormat" else I18n.t "hmi.text"
            add (labelled textLabel (textBox vm.Text (fun v -> vm.Text <- v)))

            // 버튼 기능: 연결한 요소의 동작을 이 부품에서만 덮어쓴다.
            // 같은 코일에 '운전'(ON)과 '정지'(OFF) 버튼을 따로 둘 때 쓴다.
            if HmiPartKind.hasAction vm.Kind then
                let actions =
                    ("", I18n.t "hmi.actionInherit")
                    :: (SwitchAction.all |> List.map (fun a -> a.Code, I18n.actionLabel a))
                add (labelled (I18n.t "hmi.action") (comboBox actions vm.Action (fun v -> vm.Action <- v)))

            if vm.Kind = PartButton || vm.Kind = PartToggle || vm.Kind = PartLamp then
                add (labelled (I18n.t "hmi.onText") (textBox vm.OnText (fun v -> vm.OnText <- v)))
                add (labelled (I18n.t "hmi.offText") (textBox vm.OffText (fun v -> vm.OffText <- v)))

            if HmiPartKind.isWordPart vm.Kind && vm.Kind <> PartRotary then
                add (labelled (I18n.t "hmi.unit") (textBox vm.Unit (fun v -> vm.Unit <- v)))

            // 눈금 범위를 비워 두면(둘 다 0) 연결한 요소의 최소~최대를 그대로 쓴다.
            if HmiPartKind.hasScale vm.Kind then
                let lo = numberBox -32768 65535 vm.ScaleMin (fun v -> vm.ScaleMin <- v)
                let hi = numberBox -32768 65535 vm.ScaleMax (fun v -> vm.ScaleMax <- v)
                lo.ShowButtonSpinner <- false
                hi.ShowButtonSpinner <- false
                let pair = Grid(ColumnSpacing = 6.0)
                pair.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                pair.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                Grid.SetColumn(lo, 0)
                Grid.SetColumn(hi, 1)
                pair.Children.Add lo
                pair.Children.Add hi
                add (labelled (I18n.t "hmi.scale") (pair :> Control))

            if HmiPartKind.hasDecimals vm.Kind then
                add (labelled (I18n.t "hmi.decimals") (numberBox 0 3 vm.Decimals (fun v -> vm.Decimals <- v)))

            if vm.Kind = PartValue then
                add (labelled (I18n.t "hmi.step") (numberBox 0 10000 vm.Step (fun v -> vm.Step <- v)))

            // 삼각 버튼은 증감폭의 부호가 방향이다. (양수 ▲ / 음수 ▼)
            if vm.Kind = PartArrow then
                add (labelled (I18n.t "hmi.step") (numberBox -10000 10000 vm.Step (fun v -> vm.Step <- v)))

            if vm.Kind = PartSetValue then
                add (labelled (I18n.t "hmi.writeValue") (numberBox -32768 65535 vm.WriteValue (fun v -> vm.WriteValue <- v)))

            if vm.Kind = PartRotary || vm.Kind = PartLampArray then
                add (labelled (I18n.t "hmi.count") (numberBox 1 16 vm.Count (fun v -> vm.Count <- v)))

            if vm.Kind = PartRotary then
                add (labelled (I18n.t "hmi.options") (textBox vm.Options (fun v -> vm.Options <- v)))

            if HmiPartKind.hasOrientation vm.Kind then
                add (checkBox (I18n.t "hmi.vertical") vm.Vertical (fun v -> vm.Vertical <- v))

            // 같은 이름을 적은 버튼끼리 한 번에 하나만 켜진다.
            // 누르면 같은 그룹의 다른 코일을 먼저 OFF 로 쓴다.
            if HmiPartKind.hasGroup vm.Kind then
                add (labelled (I18n.t "hmi.group") (textBox vm.Group (fun v -> vm.Group <- v)))

            // '재시작' 처럼 명령을 낸 뒤 다른 버튼이 켜져 있어야 하는 경우에 쓴다.
            if HmiPartKind.hasAction vm.Kind then
                add (labelled (I18n.t "hmi.thenOn")
                        (targetCombo PartButton vm.ThenOnId (fun id -> vm.ThenOnId <- id)))

            add (sectionGap ())

            if HmiPartKind.hasShape vm.Kind then
                let shapes =
                    [ HmiShape.rect, "▭"
                      HmiShape.round, "▬"
                      HmiShape.circle, "●" ]
                add (labelled (I18n.t "hmi.shape") (comboBox shapes vm.Shape (fun v -> vm.Shape <- v)))

            let roundable =
                vm.Kind = PartPanel || vm.Kind = PartLampArray || vm.Kind = PartBar
                || vm.Kind = PartArrow || vm.Kind = PartValue
                || (HmiPartKind.hasShape vm.Kind && vm.Shape = HmiShape.rect)
            if roundable then
                add (labelled (I18n.t "hmi.corner") (numberBox 0 60 vm.Corner (fun v -> vm.Corner <- v)))

            let plainText = vm.Kind = PartLabel || vm.Kind = PartClock
            if not plainText && vm.Kind <> PartGauge then
                add (labelled (I18n.t "hmi.offColor") (colorBox vm.OffColor (fun v -> vm.OffColor <- v)))
            let lightsUp =
                HmiPartKind.isBitPart vm.Kind || vm.Kind = PartGauge || vm.Kind = PartBar
                || vm.Kind = PartArrow || vm.Kind = PartSetValue || vm.Kind = PartRotary
            if lightsUp then
                add (labelled (I18n.t "hmi.onColor") (colorBox vm.OnColor (fun v -> vm.OnColor <- v)))
            add (labelled (I18n.t "hmi.textColor") (colorBox vm.TextColor (fun v -> vm.TextColor <- v)))
            if not plainText then
                add (labelled (I18n.t "hmi.borderColor") (colorBox vm.BorderColor (fun v -> vm.BorderColor <- v)))

            add (labelled (I18n.t "hmi.fontSize")
                    (numberBox HmiLimits.minFontSize HmiLimits.maxFontSize vm.FontSize (fun v -> vm.FontSize <- v)))

            if vm.Kind = PartLabel || vm.Kind = PartPanel || vm.Kind = PartClock then
                let aligns = [ "LEFT", "⇤"; "CENTER", "↔"; "RIGHT", "⇥" ]
                add (labelled (I18n.t "hmi.align") (comboBox aligns vm.Align (fun v -> vm.Align <- v)))

            add (sectionGap ())

            xBox <- numberBox 0 HmiLimits.maxScreen vm.X (fun v -> vm.X <- v)
            yBox <- numberBox 0 HmiLimits.maxScreen vm.Y (fun v -> vm.Y <- v)
            wBox <- numberBox HmiLimits.minPartWidth HmiLimits.maxScreen vm.Width (fun v -> vm.Width <- v)
            hBox <- numberBox HmiLimits.minPartHeight HmiLimits.maxScreen vm.Height (fun v -> vm.Height <- v)
            // 두 칸이 나란히 들어가야 해서 좁다. 화살표를 빼야 숫자가 보인다. (캔버스에서 끌어 맞추는 게 주된 길이다)
            for box in [ xBox; yBox; wBox; hBox ] do
                box.ShowButtonSpinner <- false

            let pair (a: Control) (b: Control) =
                let g = Grid(ColumnSpacing = 6.0)
                g.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                g.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                Grid.SetColumn(a, 0)
                Grid.SetColumn(b, 1)
                g.Children.Add a
                g.Children.Add b
                g :> Control

            add (labelled "X / Y" (pair xBox yBox))
            add (labelled "W / H" (pair wBox hBox))

            add (sectionGap ())

            let buttons =
                Ui.stackH 5.0
                    [ Ui.toolButton (I18n.t "cmd.duplicate") "" (fun () -> state.DuplicatePart vm |> ignore)
                      Ui.button (I18n.t "cmd.delete") [ "danger" ] (fun () -> state.RemovePart vm |> ignore) ]
            add (buttons :> Control)

        suppress <- false

    /// 캔버스에서 끌어 옮기는 동안 위치/크기 칸을 따라가게 한다.
    let syncBounds (vm: HmiPartVm) (prop: string) =
        if state.IsPartSelected vm then
            suppress <- true
            let set (box: NumericUpDown) (v: int) =
                if not (isNull box) && box.Value <> Nullable(decimal v) then box.Value <- decimal v
            match prop with
            | "X" -> set xBox vm.X
            | "Y" -> set yBox vm.Y
            | "Width" -> set wBox vm.Width
            | "Height" -> set hBox vm.Height
            | _ -> ()
            suppress <- false

    // ---------------------------------------------------------------- 툴바
    let buildToolbar () =
        let wrap = WrapPanel(Orientation = Orientation.Horizontal, Margin = Thickness(8.0, 5.0, 8.0, 5.0), ItemSpacing = 4.0, LineSpacing = 4.0)

        let addMenu =
            Ui.menuButton
                (I18n.t "hmi.addPart")
                ""
                (HmiPartKind.all
                 |> List.map (fun kind ->
                     Ui.menuItem (I18n.partLabel kind) (fun () ->
                         let vm = state.AddPart kind
                         notify (sprintf "HMI ADD %s" vm.Kind.Code))
                     :> Control))

        let fromElements =
            Ui.toolButton (I18n.t "hmi.fromElements") "" (fun () ->
                let made = state.AddPartsFromElements state.Selection
                if made = 0 then notify (I18n.t "msg.selectFirst")
                else notify (sprintf "HMI ADD FROM ELEMENTS %d" made))

        let editToggle =
            Ui.toggleButton (I18n.t "hmi.edit") [ "warn" ] canvas.EditMode (fun isChecked ->
                canvas.EditMode <- isChecked
                setHint ()
                notify ("HMI EDIT " + (if isChecked then "ON" else "OFF")))

        let gridToggle =
            Ui.toggleButton (I18n.t "cmd.showGrid") [] canvas.ShowGrid (fun isChecked -> canvas.ShowGrid <- isChecked)

        let snapToggle =
            Ui.toggleButton (I18n.t "cmd.snap") [] canvas.SnapToGrid (fun isChecked -> canvas.SnapToGrid <- isChecked)

        widthBox <-
            NumericUpDown(
                Minimum = decimal HmiLimits.minScreen,
                Maximum = decimal HmiLimits.maxScreen,
                Value = decimal state.HmiWidth,
                Increment = 8m,
                FormatString = "0",
                Width = 124.0
            )
        heightBox <-
            NumericUpDown(
                Minimum = decimal HmiLimits.minScreen,
                Maximum = decimal HmiLimits.maxScreen,
                Value = decimal state.HmiHeight,
                Increment = 8m,
                FormatString = "0",
                Width = 124.0
            )
        widthBox.ValueChanged.Add(fun _ -> if widthBox.Value.HasValue then state.HmiWidth <- int widthBox.Value.Value)
        heightBox.ValueChanged.Add(fun _ -> if heightBox.Value.HasValue then state.HmiHeight <- int heightBox.Value.Value)

        let presets =
            Ui.menuButton
                (I18n.t "hmi.screenSize")
                ""
                (HmiLimits.presets
                 |> List.map (fun (w, h) ->
                     Ui.menuItem (sprintf "%d × %d" w h) (fun () ->
                         state.HmiWidth <- w
                         state.HmiHeight <- h)
                     :> Control))

        let background =
            let combo = colorBox state.HmiBackground (fun hex -> state.HmiBackground <- hex)
            combo.Width <- 152.0
            combo

        zoomLabel <- Ui.mono 11.5 "100%"
        zoomLabel.Foreground <- Ui.brush p.TextMuted

        let setZoom (v: float) =
            canvas.Zoom <- v
            zoomLabel.Text <- sprintf "%d%%" (int (canvas.Zoom * 100.0))

        let labelled (caption: string) (editor: Control) =
            let l = Ui.muted caption
            l.FontSize <- 11.0
            Ui.stackH 6.0 [ l; editor ] :> Control

        let items: Control list =
            [ Ui.stackH 5.0 [ addMenu; fromElements ] :> Control
              Ui.vSep ()
              Ui.stackH 5.0 [ editToggle; gridToggle; snapToggle ] :> Control
              Ui.vSep ()
              Ui.stackH 5.0 [ presets; widthBox; Ui.text "×" :> Control; heightBox ] :> Control
              labelled (I18n.t "hmi.background") background
              Ui.vSep ()
              Ui.stackH 5.0
                  [ Ui.toolButton (I18n.t "cmd.fitToWindow") "" (fun () ->
                        canvas.FitToWindow()
                        zoomLabel.Text <- sprintf "%d%%" (int (canvas.Zoom * 100.0)))
                    Ui.toolButton "−" (I18n.t "cmd.zoomOut") (fun () -> setZoom (canvas.Zoom - 0.1))
                    zoomLabel
                    Ui.toolButton "＋" (I18n.t "cmd.zoomIn") (fun () -> setZoom (canvas.Zoom + 0.1))
                    Ui.toolButton "1:1" (I18n.t "cmd.zoomReset") (fun () -> setZoom 1.0) ]
              :> Control ]

        for i in items do
            wrap.Children.Add i

        Border(Background = Ui.brush p.Header, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(0.0, 0.0, 0.0, 1.0), Child = wrap)

    // ---------------------------------------------------------------- 조립
    let root =
        let inspector =
            let d = DockPanel(LastChildFill = true)
            let header = Ui.panelHeader (I18n.t "hmi.partProps") None
            DockPanel.SetDock(header, Dock.Top)
            d.Children.Add header
            d.Children.Add(ScrollViewer(Content = inspectorBody, VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto))
            Border(Background = Ui.brush p.Surface, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(1.0, 0.0, 0.0, 0.0), Child = d)

        hintText.FontSize <- 11.0
        hintText.Margin <- Thickness(10.0, 4.0, 10.0, 4.0)
        hintText.TextWrapping <- TextWrapping.Wrap

        let center = DockPanel(LastChildFill = true)
        let toolbar = buildToolbar ()
        DockPanel.SetDock(toolbar, Dock.Top)
        center.Children.Add toolbar
        DockPanel.SetDock(hintBar, Dock.Top)
        center.Children.Add hintBar
        center.Children.Add canvas.Root

        let body = Grid()
        body.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
        body.ColumnDefinitions.Add(ColumnDefinition(GridLength(4.0, GridUnitType.Pixel)))
        body.ColumnDefinitions.Add(ColumnDefinition(GridLength(272.0, GridUnitType.Pixel), MinWidth = 190.0))
        Grid.SetColumn(center, 0)
        body.Children.Add center
        let splitter = GridSplitter(Width = 4.0, ResizeDirection = GridResizeDirection.Columns)
        Grid.SetColumn(splitter, 1)
        body.Children.Add splitter
        Grid.SetColumn(inspector, 2)
        body.Children.Add inspector
        body

    do
        canvas.Rebuild()
        // 탭을 열자마자 터치패널 전체가 보이게 맞춘다.
        canvas.FitWhenReady()
        setHint ()
        rebuildInspector ()

        canvas.ZoomChanged.Add(fun z -> if not (isNull zoomLabel) then zoomLabel.Text <- sprintf "%d%%" (int (z * 100.0)))

        subscriptions.Add(state.HmiSelectionChanged.Subscribe(fun () -> rebuildInspector ()))
        // 요소가 늘거나 줄면 '연결 요소' 목록도 따라가야 한다.
        subscriptions.Add(state.StructureChanged.Subscribe(fun () -> rebuildInspector ()))
        subscriptions.Add(state.HmiPartChanged.Subscribe(fun (vm, prop) -> syncBounds vm prop))
        subscriptions.Add(
            state.HmiScreenChanged.Subscribe(fun () ->
                if not (isNull widthBox) && widthBox.Value <> Nullable(decimal state.HmiWidth) then
                    widthBox.Value <- decimal state.HmiWidth
                if not (isNull heightBox) && heightBox.Value <> Nullable(decimal state.HmiHeight) then
                    heightBox.Value <- decimal state.HmiHeight))

    member _.Root: Control = root :> Control

    member _.Canvas = canvas

    interface IDisposable with
        member _.Dispose() =
            for s in subscriptions do
                s.Dispose()
            subscriptions.Clear()
            (canvas :> IDisposable).Dispose()
