/// 운전 화면 카드(스위치/램프/숫자/텍스트)를 만든다.
module XgbHmi.App.Views.CardFactory

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Shapes
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.App.Themes
open XgbHmi.App.ViewModels

/// 카드에서 나오는 조작을 화면 쪽으로 넘기는 통로
type CardCallbacks =
    { Toggle: ElementVm -> unit
      WriteOn: ElementVm -> unit
      WriteOff: ElementVm -> unit
      MomentaryDown: ElementVm -> unit
      MomentaryUp: ElementVm -> unit
      NumericWrite: ElementVm -> int -> unit
      /// 배치 편집 중이면 false. PLC로 명령을 보내지 않는다.
      IsInteractive: unit -> bool }

/// 한 번 갱신할 때 카드가 참고하는 실행 상태 한 벌
type RuntimeStatus =
    { BitOf: string -> bool option
      WordOf: string -> uint16 option
      /// PLC 통신 자체가 오류면 모든 카드를 빨간색으로 점등한다.
      CommFault: bool }

/// 캔버스에 올라간 카드 하나
type RuntimeCard =
    { Vm: ElementVm
      Root: Border
      /// 최신 PLC 값으로 카드 표시를 갱신한다.
      Refresh: RuntimeStatus -> unit }

let private kindColor (p: Palette) (kind: ItemKind) =
    match kind with
    | Switch -> p.KindSwitch
    | Lamp -> p.KindLamp
    | NumInput
    | NumDisplay -> p.KindNumeric
    | Text -> p.KindText

/// 지정한 색으로 발광을 만든다. 테마의 Glow 설정과 무관하게 점등은 항상 보여야 한다.
let private lightGlow (hex: string) (blur: float) =
    let c = Color.Parse hex
    BoxShadows.Parse(sprintf "0 0 %g 0 %s" blur ((Color.FromArgb(140uy, c.R, c.G, c.B)).ToString()))

/// 점등된 버튼 위의 글자색. 밝은 색 위에는 검은 글자, 어두운 색 위에는 흰 글자를 얹는다.
let private textOnLight (hex: string) =
    let c = Color.Parse hex
    let luma = (0.299 * float c.R + 0.587 * float c.G + 0.114 * float c.B) / 255.0
    if luma > 0.62 then "#101318" else "#FFFFFF"

/// 카드 한 장을 만든다.
let create (p: Palette) (vm: ElementVm) (cb: CardCallbacks) : RuntimeCard =
    let accent = kindColor p vm.Kind

    let root =
        Border(
            Background = Ui.brush p.CardBg,
            BorderBrush = Ui.brush p.CardBorder,
            BorderThickness = Thickness 1.0,
            CornerRadius = CornerRadius 8.0,
            ClipToBounds = true,
            Width = float vm.Width,
            Height = float vm.Height,
            Tag = vm.Id
        )

    // 왼쪽 종류별 색 띠
    let stripe = Border(Width = 4.0, Background = Ui.brush accent, HorizontalAlignment = HorizontalAlignment.Left)

    let layout = Grid(Margin = Thickness(10.0, 5.0, 9.0, 6.0))
    layout.RowDefinitions.Add(RowDefinition(GridLength.Auto))
    layout.RowDefinitions.Add(RowDefinition(GridLength.Auto))
    layout.RowDefinitions.Add(RowDefinition(GridLength.Auto))
    layout.RowDefinitions.Add(RowDefinition(GridLength(1.0, GridUnitType.Star)))

    // ---- 제목 줄 ----
    let header = DockPanel(LastChildFill = true)
    let deviceTag = Ui.mono 10.5 vm.Device
    deviceTag.Foreground <- Ui.brush p.TextMuted
    deviceTag.Margin <- Thickness(6.0, 0.0, 0.0, 0.0)
    DockPanel.SetDock(deviceTag, Dock.Right)
    header.Children.Add deviceTag

    let nameText = Ui.title 12.0 vm.Name
    nameText.TextTrimming <- TextTrimming.CharacterEllipsis
    nameText.Foreground <- Ui.brush p.Text
    header.Children.Add nameText
    Grid.SetRow(header, 0)
    layout.Children.Add header

    // ---- 상태 표시 ----
    let stateText = Ui.text (I18n.t "state.unknown")
    stateText.FontSize <- 12.0
    stateText.FontWeight <- FontWeight.SemiBold
    stateText.HorizontalAlignment <- HorizontalAlignment.Center
    stateText.Foreground <- Ui.brush p.TextMuted

    let statePill =
        Border(
            Background = Ui.tint p.Off 0.18,
            CornerRadius = CornerRadius 5.0,
            Padding = Thickness(5.0, 1.0),
            Margin = Thickness(0.0, 4.0, 0.0, 0.0),
            Child = stateText
        )

    let monitorText = Ui.text ""
    monitorText.FontSize <- 11.0
    monitorText.HorizontalAlignment <- HorizontalAlignment.Center
    monitorText.Foreground <- Ui.brush p.TextMuted
    monitorText.Margin <- Thickness(0.0, 2.0, 0.0, 0.0)

    // ---- 종류별 본문 ----
    let mutable numericBox: NumericUpDown = null
    let mutable bigValue: TextBlock = null

    let interactive () = cb.IsInteractive ()

    // ---- 조작 버튼 라이팅 ----
    // 버튼에는 그림자를 줄 수 없어 테두리로 한 겹 감싸고 그 테두리를 빛나게 한다.
    let mutable onLamp: (Border * Button) option = None
    let mutable offLamp: (Border * Button) option = None
    let mutable actionLamp: (Border * Button) option = None
    /// 순간 스위치를 누르고 있는 동안은 PLC 응답을 기다리지 않고 바로 점등한다.
    let held = ref false

    let makeLamp (b: Button) =
        Border(CornerRadius = CornerRadius 5.0, Background = Brushes.Transparent, Child = b)

    /// 버튼을 지정한 색으로 점등하거나(Some) 원래 모습으로 되돌린다(None).
    let light (lamp: (Border * Button) option) (color: string option) =
        match lamp with
        | None -> ()
        | Some(host, b) ->
            match color with
            | Some c ->
                b.Background <- Ui.brush c
                b.Foreground <- Ui.brush (textOnLight c)
                b.BorderBrush <- Ui.brush c
                host.BoxShadow <- lightGlow c 15.0
            | None ->
                b.ClearValue Primitives.TemplatedControl.BackgroundProperty
                b.ClearValue Primitives.TemplatedControl.ForegroundProperty
                b.ClearValue Primitives.TemplatedControl.BorderBrushProperty
                host.BoxShadow <- BoxShadows()

    match vm.Kind with
    | Text ->
        let t = TextBlock(Text = vm.Name, TextWrapping = TextWrapping.Wrap, FontFamily = Ui.uiFont, FontSize = 13.0, FontWeight = FontWeight.SemiBold)
        t.Foreground <- Ui.brush p.Text
        t.VerticalAlignment <- VerticalAlignment.Center
        layout.Children.Remove header |> ignore
        Grid.SetRow(t, 0)
        Grid.SetRowSpan(t, 4)
        layout.Children.Add t

    | Switch ->
        Grid.SetRow(statePill, 1)
        layout.Children.Add statePill
        if not (String.IsNullOrWhiteSpace vm.MonitorDevice) then
            Grid.SetRow(monitorText, 2)
            layout.Children.Add monitorText

        let controlArea =
            match vm.Action with
            | OnOff ->
                let g = Grid(Margin = Thickness(0.0, 5.0, 0.0, 0.0), MinHeight = 28.0)
                g.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                g.ColumnDefinitions.Add(ColumnDefinition(GridLength(6.0, GridUnitType.Pixel)))
                g.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                let on = Ui.button (I18n.t "action.on") [ "hmi" ] (fun () -> if interactive () then cb.WriteOn vm)
                let off = Ui.button (I18n.t "action.off") [ "hmi" ] (fun () -> if interactive () then cb.WriteOff vm)
                let onHost = makeLamp on
                let offHost = makeLamp off
                onHost.VerticalAlignment <- VerticalAlignment.Center
                offHost.VerticalAlignment <- VerticalAlignment.Center
                onLamp <- Some(onHost, on)
                offLamp <- Some(offHost, off)
                Grid.SetColumn(onHost, 0)
                Grid.SetColumn(offHost, 2)
                g.Children.Add onHost
                g.Children.Add offHost
                g :> Control
            | Momentary ->
                let b = Ui.button (I18n.t "action.momentary") [ "hmi" ] (fun () -> ())
                b.MinHeight <- 28.0
                b.VerticalAlignment <- VerticalAlignment.Stretch
                let host = makeLamp b
                host.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
                let lamp = Some(host, b)
                actionLamp <- lamp
                // 누른 즉시 점등하고, 뗀 뒤에는 다음 갱신이 실제 비트로 다시 맞춘다.
                let release () =
                    if held.Value then
                        held.Value <- false
                        light lamp None
                        if interactive () then cb.MomentaryUp vm
                b.AddHandler(
                    InputElement.PointerPressedEvent,
                    (fun _ (e: PointerPressedEventArgs) ->
                        if interactive () && e.GetCurrentPoint(b).Properties.IsLeftButtonPressed then
                            held.Value <- true
                            light lamp (Some p.On)
                            cb.MomentaryDown vm),
                    Interactivity.RoutingStrategies.Tunnel
                )
                b.AddHandler(
                    InputElement.PointerReleasedEvent,
                    (fun _ (_: PointerReleasedEventArgs) -> release ()),
                    Interactivity.RoutingStrategies.Tunnel
                )
                b.PointerExited.Add(fun _ -> release ())
                host :> Control
            | action ->
                let label =
                    match action with
                    | Toggle -> I18n.t "action.toggle"
                    | On -> I18n.t "action.on"
                    | Off -> I18n.t "action.off"
                    | _ -> I18n.t "action.toggle"
                let b = Ui.button label [ "hmi" ] (fun () -> if interactive () then cb.Toggle vm)
                b.MinHeight <- 28.0
                b.VerticalAlignment <- VerticalAlignment.Stretch
                let host = makeLamp b
                host.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
                actionLamp <- Some(host, b)
                host :> Control

        Grid.SetRow(controlArea, 3)
        layout.Children.Add controlArea

    | Lamp ->
        let dot = Ellipse(Width = 16.0, Height = 16.0, Fill = Ui.brush p.Off)
        let lampText = Ui.title 17.0 (I18n.t "state.off")
        lampText.Foreground <- Ui.brush p.TextMuted
        bigValue <- lampText
        let row = Ui.stackH 9.0 [ dot; lampText ]
        row.HorizontalAlignment <- HorizontalAlignment.Center
        row.VerticalAlignment <- VerticalAlignment.Center
        let holder =
            Border(
                Background = Ui.tint p.Off 0.12,
                CornerRadius = CornerRadius 6.0,
                Margin = Thickness(0.0, 5.0, 0.0, 0.0),
                Child = row
            )
        Grid.SetRow(holder, 1)
        Grid.SetRowSpan(holder, 3)
        layout.Children.Add holder
        // 램프는 상태 색을 원과 배경으로 보여준다.
        root.Tag <- vm.Id
        statePill.Child <- null
        statePill.Tag <- box (dot, holder)

    | NumDisplay ->
        let value = Ui.mono 22.0 (I18n.t "state.unknown")
        value.HorizontalAlignment <- HorizontalAlignment.Center
        value.VerticalAlignment <- VerticalAlignment.Center
        value.FontWeight <- FontWeight.Bold
        value.Foreground <- Ui.brush p.Text
        bigValue <- value
        let holder =
            Border(
                Background = Ui.tint p.KindNumeric 0.10,
                CornerRadius = CornerRadius 6.0,
                Margin = Thickness(0.0, 5.0, 0.0, 0.0),
                Child = value
            )
        Grid.SetRow(holder, 1)
        Grid.SetRowSpan(holder, 2)
        layout.Children.Add holder
        Grid.SetRow(monitorText, 3)
        monitorText.HorizontalAlignment <- HorizontalAlignment.Center
        layout.Children.Add monitorText

    | NumInput ->
        Grid.SetRow(statePill, 1)
        layout.Children.Add statePill

        let numeric =
            NumericUpDown(
                Minimum = decimal vm.Min,
                Maximum = decimal vm.Max,
                Value = decimal (max vm.Min 0),
                Increment = 1m,
                FormatString = "0",
                FontFamily = Ui.monoFont,
                FontWeight = FontWeight.Bold,
                Margin = Thickness(0.0, 5.0, 0.0, 0.0),
                MinHeight = 28.0
            )
        numericBox <- numeric

        let writeButton =
            Ui.button (I18n.t "btn.write") [ "hmi"; "primary" ] (fun () ->
                if interactive () then
                    let v = if numeric.Value.HasValue then int numeric.Value.Value else 0
                    cb.NumericWrite vm v)
        writeButton.MinHeight <- 28.0
        writeButton.VerticalAlignment <- VerticalAlignment.Stretch
        let writeHost = makeLamp writeButton
        writeHost.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
        actionLamp <- Some(writeHost, writeButton)

        let g = Grid()
        g.RowDefinitions.Add(RowDefinition(GridLength.Auto))
        g.RowDefinitions.Add(RowDefinition(GridLength(1.0, GridUnitType.Star)))
        Grid.SetRow(numeric, 0)
        Grid.SetRow(writeHost, 1)
        g.Children.Add numeric
        g.Children.Add writeHost
        Grid.SetRow(g, 2)
        Grid.SetRowSpan(g, 2)
        layout.Children.Add g

    let content = Grid()
    content.Children.Add stripe
    content.Children.Add layout
    root.Child <- content

    // ---- 오류 점등 ----
    // 테두리는 캔버스의 선택 표시가 쓰므로 건드리지 않고, 카드 바탕과 색 띠를 빨갛게 물들인다.
    let faultShown = ref false

    let showFault (message: string) =
        if not faultShown.Value then
            faultShown.Value <- true
            root.Background <- Ui.tint p.Error 0.16
            root.BoxShadow <- lightGlow p.Error 14.0
            stripe.Background <- Ui.brush p.Error
            stripe.Width <- 6.0
            nameText.Foreground <- Ui.brush p.Error
            deviceTag.Foreground <- Ui.brush p.Error
        ToolTip.SetTip(root, message)

    let hideFault () =
        if faultShown.Value then
            faultShown.Value <- false
            root.Background <- Ui.brush p.CardBg
            root.BoxShadow <- BoxShadows()
            stripe.Background <- Ui.brush accent
            stripe.Width <- 4.0
            nameText.Foreground <- Ui.brush p.Text
            deviceTag.Foreground <- Ui.brush p.TextMuted
            ToolTip.SetTip(root, null)

    /// 이 카드가 지금 오류인지. 요소별 쓰기/읽기 실패가 우선이고, 없으면 통신 전체 오류를 본다.
    let faultOf (status: RuntimeStatus) =
        match vm.Fault with
        | Some m -> Some m
        | None -> if status.CommFault then Some(I18n.t "status.error") else None

    // ---- PLC 값 반영 ----
    let refresh (status: RuntimeStatus) =
        let bitOf = status.BitOf
        let wordOf = status.WordOf
        match vm.Kind with
        | Text -> ()
        | Switch ->
            (match bitOf vm.Device with
             | Some true ->
                 stateText.Text <- vm.Device + " : ● " + I18n.t "state.on"
                 stateText.Foreground <- Ui.brush p.On
                 statePill.Background <- Ui.tint p.On 0.20
                 statePill.BoxShadow <- lightGlow p.On 12.0
             | Some false ->
                 stateText.Text <- vm.Device + " : ○ " + I18n.t "state.off"
                 stateText.Foreground <- Ui.brush p.TextMuted
                 statePill.Background <- Ui.tint p.Off 0.18
                 statePill.BoxShadow <- BoxShadows()
             | None ->
                 stateText.Text <- vm.Device + " : " + I18n.t "state.unknown"
                 stateText.Foreground <- Ui.brush p.TextMuted
                 statePill.Background <- Ui.tint p.Off 0.18
                 statePill.BoxShadow <- BoxShadows())

            if not (String.IsNullOrWhiteSpace vm.MonitorDevice) then
                match bitOf vm.MonitorDevice with
                | Some true ->
                    monitorText.Text <- vm.MonitorDevice + " : ● " + I18n.t "state.on"
                    monitorText.Foreground <- Ui.brush p.On
                | Some false ->
                    monitorText.Text <- vm.MonitorDevice + " : ○ " + I18n.t "state.off"
                    monitorText.Foreground <- Ui.brush p.TextMuted
                | None ->
                    monitorText.Text <- vm.MonitorDevice + " : " + I18n.t "state.unknown"
                    monitorText.Foreground <- Ui.brush p.TextMuted

            // 동작 중인 버튼을 점등한다. 상태확인 디바이스가 있으면 그쪽 값을 먼저 본다.
            let live =
                if String.IsNullOrWhiteSpace vm.MonitorDevice then bitOf vm.Device
                else
                    match bitOf vm.MonitorDevice with
                    | Some v -> Some v
                    | None -> bitOf vm.Device
            let isOn = live = Some true || held.Value
            let isOff = live = Some false && not held.Value
            match vm.Action with
            | OnOff ->
                light onLamp (if isOn then Some p.On else None)
                light offLamp (if isOff then Some p.Off else None)
            | Off -> light actionLamp (if isOff then Some p.Off else None)
            | _ -> light actionLamp (if isOn then Some p.On else None)

        | Lamp ->
            match statePill.Tag with
            | :? (Ellipse * Border) as pair ->
                let dot, holder = pair
                match bitOf vm.Device with
                | Some true ->
                    dot.Fill <- Ui.brush p.On
                    holder.Background <- Ui.tint p.On 0.18
                    holder.BoxShadow <- lightGlow p.On 16.0
                    if not (isNull bigValue) then
                        bigValue.Text <- I18n.t "state.on"
                        bigValue.Foreground <- Ui.brush p.On
                | Some false ->
                    dot.Fill <- Ui.brush p.Off
                    holder.Background <- Ui.tint p.Off 0.12
                    holder.BoxShadow <- BoxShadows()
                    if not (isNull bigValue) then
                        bigValue.Text <- I18n.t "state.off"
                        bigValue.Foreground <- Ui.brush p.TextMuted
                | None ->
                    dot.Fill <- Ui.brush p.Off
                    holder.Background <- Ui.tint p.Off 0.12
                    holder.BoxShadow <- BoxShadows()
                    if not (isNull bigValue) then
                        bigValue.Text <- I18n.t "state.unknown"
                        bigValue.Foreground <- Ui.brush p.TextMuted
            | _ -> ()

        | NumDisplay ->
            match wordOf vm.Device with
            | Some w ->
                let signed = int16 w
                if not (isNull bigValue) then
                    bigValue.Text <- string w
                    bigValue.Foreground <- Ui.brush p.Text
                monitorText.Text <- sprintf "%s = %d  (signed %d)" vm.Device w signed
                monitorText.Foreground <- Ui.brush p.TextMuted
            | None ->
                if not (isNull bigValue) then
                    bigValue.Text <- I18n.t "state.unknown"
                    bigValue.Foreground <- Ui.brush p.Text
                monitorText.Text <- vm.Device + " = " + I18n.t "state.unknown"
                monitorText.Foreground <- Ui.brush p.TextMuted

        | NumInput ->
            match wordOf vm.Device with
            | Some w ->
                let signed = int16 w
                stateText.Text <- sprintf "%s = %d  (signed %d)" vm.Device w signed
                stateText.Foreground <- Ui.brush p.Text
                statePill.Background <- Ui.tint p.KindNumeric 0.12
                statePill.BoxShadow <- BoxShadows()
            | None ->
                stateText.Text <- vm.Device + " = " + I18n.t "state.unknown"
                stateText.Foreground <- Ui.brush p.TextMuted
                statePill.Background <- Ui.tint p.Off 0.18
                statePill.BoxShadow <- BoxShadows()

            light actionLamp None

        // ---- 오류면 위 표시 위에 빨간색 점등을 덮어쓴다 ----
        match faultOf status with
        | None -> hideFault ()
        | Some message ->
            showFault message
            let red = Ui.brush p.Error
            let fault = I18n.t "state.fault"
            match vm.Kind with
            | Text -> ()
            | Lamp ->
                match statePill.Tag with
                | :? (Ellipse * Border) as pair ->
                    let dot, holder = pair
                    dot.Fill <- red
                    holder.Background <- Ui.tint p.Error 0.20
                    holder.BoxShadow <- lightGlow p.Error 16.0
                    if not (isNull bigValue) then
                        bigValue.Text <- fault
                        bigValue.Foreground <- red
                | _ -> ()
            | Switch ->
                stateText.Text <- vm.Device + " : ▲ " + fault
                stateText.Foreground <- red
                statePill.Background <- Ui.tint p.Error 0.22
                statePill.BoxShadow <- lightGlow p.Error 14.0
                // 조작 버튼도 빨간색으로 점등해 어느 카드가 오류인지 바로 보이게 한다.
                light onLamp (Some p.Error)
                light offLamp (Some p.Error)
                light actionLamp (Some p.Error)
            | NumDisplay ->
                if not (isNull bigValue) then
                    bigValue.Text <- fault
                    bigValue.Foreground <- red
                monitorText.Text <- vm.Device + " : " + message
                monitorText.Foreground <- red
            | NumInput ->
                stateText.Text <- vm.Device + " : " + fault
                stateText.Foreground <- red
                statePill.Background <- Ui.tint p.Error 0.22
                statePill.BoxShadow <- lightGlow p.Error 14.0
                light actionLamp (Some p.Error)

    { Vm = vm; Root = root; Refresh = refresh }
