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

/// 캔버스에 올라간 카드 하나
type RuntimeCard =
    { Vm: ElementVm
      Root: Border
      /// 최신 PLC 값으로 카드 표시를 갱신한다.
      Refresh: (string -> bool option) -> (string -> uint16 option) -> unit }

let private kindColor (p: Palette) (kind: ItemKind) =
    match kind with
    | Switch -> p.KindSwitch
    | Lamp -> p.KindLamp
    | NumInput
    | NumDisplay -> p.KindNumeric
    | Text -> p.KindText

let private glowShadow (p: Palette) =
    if p.Glow.StartsWith "#00" then BoxShadows()
    else BoxShadows.Parse("0 0 16 0 " + p.Glow)

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
                let on = Ui.button (I18n.t "action.on") [ "hmi"; "primary" ] (fun () -> if interactive () then cb.WriteOn vm)
                let off = Ui.button (I18n.t "action.off") [ "hmi" ] (fun () -> if interactive () then cb.WriteOff vm)
                Grid.SetColumn(on, 0)
                Grid.SetColumn(off, 2)
                g.Children.Add on
                g.Children.Add off
                g :> Control
            | Momentary ->
                let b = Ui.button (I18n.t "action.momentary") [ "hmi" ] (fun () -> ())
                b.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
                b.MinHeight <- 28.0
                b.VerticalAlignment <- VerticalAlignment.Stretch
                b.AddHandler(
                    InputElement.PointerPressedEvent,
                    (fun _ (e: PointerPressedEventArgs) ->
                        if interactive () && e.GetCurrentPoint(b).Properties.IsLeftButtonPressed then cb.MomentaryDown vm),
                    Interactivity.RoutingStrategies.Tunnel
                )
                b.AddHandler(
                    InputElement.PointerReleasedEvent,
                    (fun _ (_: PointerReleasedEventArgs) -> if interactive () then cb.MomentaryUp vm),
                    Interactivity.RoutingStrategies.Tunnel
                )
                b.PointerExited.Add(fun _ -> if interactive () then cb.MomentaryUp vm)
                b :> Control
            | action ->
                let label =
                    match action with
                    | Toggle -> I18n.t "action.toggle"
                    | On -> I18n.t "action.on"
                    | Off -> I18n.t "action.off"
                    | _ -> I18n.t "action.toggle"
                let cls = if action = Toggle then [ "hmi"; "primary" ] else [ "hmi" ]
                let b = Ui.button label cls (fun () -> if interactive () then cb.Toggle vm)
                b.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
                b.MinHeight <- 28.0
                b.VerticalAlignment <- VerticalAlignment.Stretch
                b :> Control

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
        writeButton.Margin <- Thickness(0.0, 5.0, 0.0, 0.0)
        writeButton.MinHeight <- 28.0
        writeButton.VerticalAlignment <- VerticalAlignment.Stretch

        let g = Grid()
        g.RowDefinitions.Add(RowDefinition(GridLength.Auto))
        g.RowDefinitions.Add(RowDefinition(GridLength(1.0, GridUnitType.Star)))
        Grid.SetRow(numeric, 0)
        Grid.SetRow(writeButton, 1)
        g.Children.Add numeric
        g.Children.Add writeButton
        Grid.SetRow(g, 2)
        Grid.SetRowSpan(g, 2)
        layout.Children.Add g

    let content = Grid()
    content.Children.Add stripe
    content.Children.Add layout
    root.Child <- content

    // ---- PLC 값 반영 ----
    let refresh (bitOf: string -> bool option) (wordOf: string -> uint16 option) =
        match vm.Kind with
        | Text -> ()
        | Switch ->
            (match bitOf vm.Device with
             | Some true ->
                 stateText.Text <- vm.Device + " : ● " + I18n.t "state.on"
                 stateText.Foreground <- Ui.brush p.On
                 statePill.Background <- Ui.tint p.On 0.20
                 statePill.BoxShadow <- glowShadow p
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

        | Lamp ->
            match statePill.Tag with
            | :? (Ellipse * Border) as pair ->
                let dot, holder = pair
                match bitOf vm.Device with
                | Some true ->
                    dot.Fill <- Ui.brush p.On
                    holder.Background <- Ui.tint p.On 0.18
                    holder.BoxShadow <- glowShadow p
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
                if not (isNull bigValue) then bigValue.Text <- string w
                monitorText.Text <- sprintf "%s = %d  (signed %d)" vm.Device w signed
                monitorText.Foreground <- Ui.brush p.TextMuted
            | None ->
                if not (isNull bigValue) then bigValue.Text <- I18n.t "state.unknown"
                monitorText.Text <- vm.Device + " = " + I18n.t "state.unknown"

        | NumInput ->
            match wordOf vm.Device with
            | Some w ->
                let signed = int16 w
                stateText.Text <- sprintf "%s = %d  (signed %d)" vm.Device w signed
                stateText.Foreground <- Ui.brush p.Text
                statePill.Background <- Ui.tint p.KindNumeric 0.12
            | None ->
                stateText.Text <- vm.Device + " = " + I18n.t "state.unknown"
                stateText.Foreground <- Ui.brush p.TextMuted
                statePill.Background <- Ui.tint p.Off 0.18

    { Vm = vm; Root = root; Refresh = refresh }
