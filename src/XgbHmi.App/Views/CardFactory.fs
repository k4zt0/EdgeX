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

/// 조작 한 건이 지금 어느 단계인지
type OpPhase =
    | OpRunning
    | OpOk
    | OpFailed

/// 방금 수행했거나 수행 중인 조작. 통합 스위치가 이걸 보여 준다.
type RunningOp =
    { Name: string
      Device: string
      /// 토글 / ON / OFF / 순간 / 쓰기
      Action: string
      Phase: OpPhase
      Message: string }

/// 카드에서 나오는 조작을 화면 쪽으로 넘기는 통로
type CardCallbacks =
    { Toggle: ElementVm -> unit
      WriteOn: ElementVm -> unit
      WriteOff: ElementVm -> unit
      MomentaryDown: ElementVm -> unit
      MomentaryUp: ElementVm -> unit
      NumericWrite: ElementVm -> int -> unit
      /// 통합 스위치가 고를 수 있는 대상. 화면 편집에 있는 요소 전부다.
      Targets: unit -> ElementVm list
      /// 배치 편집 중이면 false. PLC로 명령을 보내지 않는다.
      IsInteractive: unit -> bool }

/// 한 번 갱신할 때 카드가 참고하는 실행 상태 한 벌
type RuntimeStatus =
    { BitOf: string -> bool option
      WordOf: string -> uint16 option
      /// PLC 통신 자체가 오류면 모든 카드를 빨간색으로 점등한다.
      CommFault: bool
      /// 화면 어디에서든 조작이 돌고 있으면 그 내용. 통합 스위치가 보여 준다.
      Operation: RunningOp option }

/// 캔버스에 올라간 카드 하나
type RuntimeCard =
    { Vm: ElementVm
      Root: Border
      /// 최신 PLC 값으로 카드 표시를 갱신한다.
      Refresh: RuntimeStatus -> unit }

let private kindColor (p: Palette) (kind: ItemKind) =
    match kind with
    | Switch
    | SwitchLamp
    | MasterSwitch -> p.KindSwitch
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

/// 통합 스위치의 대상 목록에 보여 줄 이름표
let private targetLabel (t: ElementVm) =
    let dev = if String.IsNullOrWhiteSpace t.Device then "-" else t.Device
    let name = if String.IsNullOrWhiteSpace t.Name then "(no name)" else t.Name
    sprintf "%s  ·  %s" name dev

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
    let mutable actionLamp: (Border * Button) option = None
    /// 램프 표시(원 + 배경). 램프와 스위치/램프가 함께 쓴다.
    let mutable lampDot: Ellipse = null
    let mutable lampHolder: Border = null
    /// 순간 스위치를 누르고 있는 동안은 PLC 응답을 기다리지 않고 바로 점등한다.
    let held = ref false
    /// 통합 스위치가 지금 겨누고 있는 대상과, 고를 수 있는 목록
    let target = ref (None: ElementVm option)
    let targetList = ref ([]: ElementVm list)
    /// 마지막으로 받은 상태. 대상을 바꿨을 때 스캔을 기다리지 않고 바로 다시 그리려고 쥐고 있는다.
    let lastStatus = ref (None: RuntimeStatus option)
    let redraw = ref (fun () -> ())
    let mutable targetBox: ComboBox = null
    let mutable targetValue: NumericUpDown = null

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

    /// 램프 표시 한 벌 (원 + 큰 글자를 담은 상자)
    let makeIndicator () =
        let dot = Ellipse(Width = 16.0, Height = 16.0, Fill = Ui.brush p.Off)
        let text = Ui.title 17.0 (I18n.t "state.off")
        text.Foreground <- Ui.brush p.TextMuted
        let row = Ui.stackH 9.0 [ dot; text ]
        row.HorizontalAlignment <- HorizontalAlignment.Center
        row.VerticalAlignment <- VerticalAlignment.Center
        let holder =
            Border(
                Background = Ui.tint p.Off 0.12,
                CornerRadius = CornerRadius 6.0,
                Margin = Thickness(0.0, 5.0, 0.0, 0.0),
                Child = row
            )
        holder, dot, text

    /// 조작 버튼 영역. 감싼 테두리를 함께 돌려주어 점등에 쓴다.
    let makeControlArea () : Control * (Border * Button) option =
        match vm.Action with
        | Momentary ->
            let b = Ui.button (I18n.t "action.momentary") [ "hmi" ] (fun () -> ())
            b.MinHeight <- 28.0
            b.VerticalAlignment <- VerticalAlignment.Stretch
            let host = makeLamp b
            host.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
            let lamp = Some(host, b)
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
            host :> Control, lamp
        | action ->
            // v6 와 같은 동작: ON 은 ON 만, OFF 는 OFF 만 쓰고, 토글만 읽어서 반전한다.
            let label, press =
                match action with
                | On -> I18n.t "action.on", fun () -> cb.WriteOn vm
                | Off -> I18n.t "action.off", fun () -> cb.WriteOff vm
                | _ -> I18n.t "action.toggle", fun () -> cb.Toggle vm
            let b = Ui.button label [ "hmi" ] (fun () -> if interactive () then press ())
            b.MinHeight <- 28.0
            b.VerticalAlignment <- VerticalAlignment.Stretch
            let host = makeLamp b
            host.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
            host :> Control, Some(host, b)

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

        let controlArea, lamp = makeControlArea ()
        actionLamp <- lamp
        Grid.SetRow(controlArea, 3)
        layout.Children.Add controlArea

    | SwitchLamp ->
        // 위는 램프(현재 상태), 아래는 조작 버튼. 한 장에서 누르고 결과를 함께 본다.
        let holder, dot, text = makeIndicator ()
        bigValue <- text
        lampDot <- dot
        lampHolder <- holder
        Grid.SetRow(holder, 1)
        Grid.SetRowSpan(holder, 2)
        layout.Children.Add holder

        let controlArea, lamp = makeControlArea ()
        actionLamp <- lamp
        Grid.SetRow(controlArea, 3)
        layout.Children.Add controlArea

    | Lamp ->
        let holder, dot, text = makeIndicator ()
        bigValue <- text
        lampDot <- dot
        lampHolder <- holder
        Grid.SetRow(holder, 1)
        Grid.SetRowSpan(holder, 3)
        layout.Children.Add holder

    | MasterSwitch ->
        // 대상 고르기 → 상태 램프 → 조작 버튼. 화면에 있는 요소를 이 한 장으로 모두 다룬다.
        let targets = cb.Targets()
        targetList.Value <- targets
        target.Value <- List.tryHead targets

        let combo =
            ComboBox(
                ItemsSource = (targets |> List.map targetLabel |> List.toArray),
                SelectedIndex = (if targets.IsEmpty then -1 else 0),
                PlaceholderText = I18n.t "master.noTarget",
                HorizontalAlignment = HorizontalAlignment.Stretch,
                Margin = Thickness(0.0, 5.0, 0.0, 0.0),
                MinHeight = 28.0,
                FontFamily = Ui.uiFont
            )
        targetBox <- combo
        Grid.SetRow(combo, 1)
        layout.Children.Add combo

        let holder, dot, text = makeIndicator ()
        bigValue <- text
        lampDot <- dot
        lampHolder <- holder
        Grid.SetRow(holder, 2)
        layout.Children.Add holder

        // WORD 대상일 때만 쓰는 값 입력칸
        let value =
            NumericUpDown(
                Minimum = -32768m,
                Maximum = 65535m,
                Value = 0m,
                Increment = 1m,
                FormatString = "0",
                FontFamily = Ui.monoFont,
                FontWeight = FontWeight.Bold,
                Margin = Thickness(0.0, 5.0, 0.0, 0.0),
                MinHeight = 28.0,
                IsVisible = false
            )
        targetValue <- value

        let b = Ui.button (I18n.t "action.toggle") [ "hmi" ] (fun () -> ())
        b.MinHeight <- 30.0
        b.VerticalAlignment <- VerticalAlignment.Stretch
        let host = makeLamp b
        host.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
        let lamp = Some(host, b)
        actionLamp <- lamp

        /// 고른 대상의 동작을 그대로 수행한다. 순간 스위치는 누름/뗌을 나눠 보낸다.
        let run () =
            match target.Value with
            | Some t when interactive () ->
                match t.Kind with
                | Switch
                | SwitchLamp ->
                    match t.Action with
                    | Toggle -> cb.Toggle t
                    | On -> cb.WriteOn t
                    | Off -> cb.WriteOff t
                    | Momentary -> ()
                | NumInput -> cb.NumericWrite t (if value.Value.HasValue then int value.Value.Value else 0)
                | _ -> ()
            | _ -> ()

        b.Click.Add(fun _ -> run ())

        // 순간 동작 대상은 누르는 동안만 ON 이어야 하므로 눌림/뗌을 직접 받는다.
        let momentaryTarget () =
            match target.Value with
            | Some t when t.Action = Momentary && (t.Kind = Switch || t.Kind = SwitchLamp) -> Some t
            | _ -> None
        let release () =
            if held.Value then
                held.Value <- false
                light lamp None
                match momentaryTarget () with
                | Some t when interactive () -> cb.MomentaryUp t
                | _ -> ()
        b.AddHandler(
            InputElement.PointerPressedEvent,
            (fun _ (e: PointerPressedEventArgs) ->
                match momentaryTarget () with
                | Some t when interactive () && e.GetCurrentPoint(b).Properties.IsLeftButtonPressed ->
                    held.Value <- true
                    light lamp (Some p.On)
                    cb.MomentaryDown t
                | _ -> ()),
            Interactivity.RoutingStrategies.Tunnel
        )
        b.AddHandler(
            InputElement.PointerReleasedEvent,
            (fun _ (_: PointerReleasedEventArgs) -> release ()),
            Interactivity.RoutingStrategies.Tunnel
        )
        b.PointerExited.Add(fun _ -> release ())

        combo.SelectionChanged.Add(fun _ ->
            let list = targetList.Value
            let i = combo.SelectedIndex
            target.Value <- (if i >= 0 && i < list.Length then Some list.[i] else None)
            // 다음 스캔을 기다리지 않고 고른 대상의 상태를 바로 보여 준다.
            redraw.Value ())

        let g = Grid()
        g.RowDefinitions.Add(RowDefinition(GridLength.Auto))
        g.RowDefinitions.Add(RowDefinition(GridLength(1.0, GridUnitType.Star)))
        Grid.SetRow(value, 0)
        Grid.SetRow(host, 1)
        g.Children.Add value
        g.Children.Add host
        Grid.SetRow(g, 3)
        layout.Children.Add g

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

    /// 스위치가 실제로 돌아온 상태. 상태확인 디바이스가 있으면 그쪽을 먼저 본다.
    let liveBit (bitOf: string -> bool option) =
        if String.IsNullOrWhiteSpace vm.MonitorDevice then bitOf vm.Device
        else
            match bitOf vm.MonitorDevice with
            | Some v -> Some v
            | None -> bitOf vm.Device

    /// 동작 중인 버튼을 점등한다. OFF 버튼은 OFF 일 때가 '지금 상태' 이다.
    let lightAction (live: bool option) =
        let isOn = live = Some true || held.Value
        let isOff = live = Some false && not held.Value
        match vm.Action with
        | Off -> light actionLamp (if isOff then Some p.Off else None)
        | _ -> light actionLamp (if isOn then Some p.On else None)

    /// 램프 표시를 지정한 색과 글자로 켠다.
    let setLamp (color: string) (glowing: bool) (caption: string) =
        if not (isNull lampDot) then
            lampDot.Fill <- Ui.brush color
            lampHolder.Background <- Ui.tint color (if glowing then 0.18 else 0.12)
            lampHolder.BoxShadow <- (if glowing then lightGlow color 16.0 else BoxShadows())
            if not (isNull bigValue) then
                bigValue.Text <- caption
                bigValue.Foreground <- Ui.brush (if glowing then color else p.TextMuted)

    /// 램프 표시를 현재 값으로 맞춘다.
    let showLamp (live: bool option) =
        if not (isNull lampDot) then
            let color, key =
                match live with
                | Some true -> p.On, "state.on"
                | Some false -> p.Off, "state.off"
                | None -> p.Off, "state.unknown"
            lampDot.Fill <- Ui.brush color
            lampHolder.Background <- Ui.tint color (if live = Some true then 0.18 else 0.12)
            lampHolder.BoxShadow <- (if live = Some true then lightGlow p.On 16.0 else BoxShadows())
            if not (isNull bigValue) then
                bigValue.Text <- I18n.t key
                bigValue.Foreground <- Ui.brush (if live = Some true then p.On else p.TextMuted)

    /// 램프 표시를 오류(빨간색)로 덮어쓴다.
    let showLampFault () =
        if not (isNull lampDot) then
            lampDot.Fill <- Ui.brush p.Error
            lampHolder.Background <- Ui.tint p.Error 0.20
            lampHolder.BoxShadow <- lightGlow p.Error 16.0
            if not (isNull bigValue) then
                bigValue.Text <- I18n.t "state.fault"
                bigValue.Foreground <- Ui.brush p.Error

    /// 이 카드가 지금 오류인지. 요소별 쓰기/읽기 실패가 우선이고, 없으면 통신 전체 오류를 본다.
    let faultOf (status: RuntimeStatus) =
        match vm.Fault with
        | Some m -> Some m
        | None -> if status.CommFault then Some(I18n.t "status.error") else None

    // ---- PLC 값 반영 ----
    let refresh (status: RuntimeStatus) =
        lastStatus.Value <- Some status
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

            lightAction (liveBit bitOf)

        | SwitchLamp ->
            // 램프는 실제로 돌아온 상태를, 버튼은 그 상태에 맞춰 점등한다.
            let live = liveBit bitOf
            showLamp live
            lightAction live

        | Lamp -> showLamp (bitOf vm.Device)

        | MasterSwitch ->
            // 화면 편집에서 요소가 늘거나 이름이 바뀌면 목록도 따라간다. 고르던 대상은 그대로 둔다.
            let latest = cb.Targets()
            if (latest |> List.map targetLabel) <> (targetList.Value |> List.map targetLabel) then
                let keep =
                    target.Value
                    |> Option.bind (fun t -> latest |> List.tryFindIndex (fun x -> x.Id = t.Id))
                targetList.Value <- latest
                if not (isNull targetBox) then
                    targetBox.ItemsSource <- latest |> List.map targetLabel |> List.toArray
                    targetBox.SelectedIndex <-
                        match keep with
                        | Some i -> i
                        | None -> if latest.IsEmpty then -1 else 0

            // 대상에 맞춰 버튼 글자와 값 입력칸을 바꾼다.
            let operable =
                match target.Value with
                | Some t ->
                    match t.Kind with
                    | Switch
                    | SwitchLamp -> true
                    | NumInput -> true
                    | _ -> false
                | None -> false
            (match actionLamp with
             | Some(_, b) ->
                 b.Content <-
                     match target.Value with
                     | None -> I18n.t "master.noTarget"
                     | Some t ->
                         match t.Kind with
                         | Switch
                         | SwitchLamp -> I18n.actionLabel t.Action
                         | NumInput -> I18n.t "btn.write"
                         | _ -> I18n.t "master.displayOnly"
                 b.IsEnabled <- operable
             | None -> ())
            if not (isNull targetValue) then
                targetValue.IsVisible <- (match target.Value with
                                          | Some t -> t.Kind = NumInput
                                          | None -> false)

            // 조작이 돌고 있으면 그 내용을, 아니면 겨누고 있는 대상의 현재 값을 보여 준다.
            // 여기서 정한 글자는 램프와 카드 이름에 함께 쓴다. (값이 바뀌면 이름도 바뀐다)
            let caption, lampColor, glowing, buttonColor, tag =
                match status.Operation with
                | Some op ->
                    // 도는 중이거나 잘 끝났으면 초록, 제대로 안 됐으면 빨강.
                    let color = if op.Phase = OpFailed then p.Error else p.On
                    let text =
                        match op.Phase with
                        | OpRunning -> sprintf "%s · %s · %s" (I18n.t "master.running") op.Name op.Action
                        | OpOk -> sprintf "%s · %s" op.Name op.Action
                        | OpFailed -> sprintf "%s · %s" op.Name (I18n.t "state.fault")
                    text, color, true, Some color, op.Device
                | None ->
                    match target.Value with
                    | None -> I18n.t "master.noTarget", p.Off, false, None, ""
                    | Some t ->
                        if ItemKind.isWord t.Kind then
                            match wordOf t.Device with
                            | Some w -> sprintf "%s · %d" t.Name (int16 w), p.KindNumeric, true, None, t.Device
                            | None -> t.Name + " · " + I18n.t "state.unknown", p.Off, false, None, t.Device
                        else
                            let live =
                                if String.IsNullOrWhiteSpace t.MonitorDevice then bitOf t.Device
                                else
                                    match bitOf t.MonitorDevice with
                                    | Some v -> Some v
                                    | None -> bitOf t.Device
                            match live with
                            | Some true -> t.Name + " · " + I18n.t "state.on", p.On, true, Some p.On, t.Device
                            | Some false -> t.Name + " · " + I18n.t "state.off", p.Off, false, None, t.Device
                            | None -> t.Name + " · " + I18n.t "state.unknown", p.Off, false, None, t.Device

            setLamp lampColor glowing caption
            light actionLamp buttonColor
            // 카드 제목도 지금 값으로 바꾼다. 멀리서도 무엇이 어떤 상태인지 읽히도록.
            nameText.Text <- caption
            deviceTag.Text <- tag
            match status.Operation with
            | Some op -> ToolTip.SetTip(root, (if String.IsNullOrWhiteSpace op.Message then null else box op.Message))
            | None -> ToolTip.SetTip(root, null)

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
            | Lamp -> showLampFault ()
            | SwitchLamp
            | MasterSwitch ->
                showLampFault ()
                light actionLamp (Some p.Error)
            | Switch ->
                stateText.Text <- vm.Device + " : ▲ " + fault
                stateText.Foreground <- red
                statePill.Background <- Ui.tint p.Error 0.22
                statePill.BoxShadow <- lightGlow p.Error 14.0
                // 조작 버튼도 빨간색으로 점등해 어느 카드가 오류인지 바로 보이게 한다.
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

    redraw.Value <- fun () -> lastStatus.Value |> Option.iter refresh

    { Vm = vm; Root = root; Refresh = refresh }
