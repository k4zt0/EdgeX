/// PLC 통신 설정 창. 이더넷(FEnet) · RS-232C · RS-485(Cnet) PLC 를 여러 대 등록한다.
module XgbHmi.App.Views.PlcDialog

open System
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.Protocol
open XgbHmi.App.Services

/// 연결 방식 이름
let kindLabel (kind: PlcLinkKind) =
    match kind with
    | LinkEthernet -> I18n.t "conn.kind.ethernet"
    | LinkRs232 -> I18n.t "conn.kind.rs232"
    | LinkRs485 -> I18n.t "conn.kind.rs485"

let parityLabel (parity: string) =
    match parity with
    | "ODD" -> I18n.t "parity.odd"
    | "EVEN" -> I18n.t "parity.even"
    | _ -> I18n.t "parity.none"

/// 목록에 보여 줄 한 줄
let private rowLabel (l: PlcLink) =
    sprintf
        "%s   %s   %s%s"
        (PlcLink.label l)
        (kindLabel l.Kind)
        (PlcLink.endpoint l)
        (if l.Enabled then "" else "   (" + I18n.t "conn.unused" + ")")

/// PLC 목록을 고치는 창을 띄우고, 창과 결과를 함께 돌려준다.
/// (창을 그대로 돌려주는 것은 화면 확인 도구가 이 창을 찍을 수 있게 하려는 것)
let editWindow (owner: Window) (existing: PlcLink list) : Window * Task<PlcLink list option> =
    let tcs = TaskCompletionSource<PlcLink list option>()
    let p = ThemeService.current ()

    let items = ResizeArray<PlcLink>(if existing.IsEmpty then [ PlcLink.empty ] else existing)
    let mutable index = 0
    let mutable suppress = false

    let list =
        ListBox(
            FontFamily = Ui.uiFont,
            Height = 168.0,
            SelectionMode = SelectionMode.Single
        )

    // ---------- 입력칸 ----------
    let nameBox = TextBox(FontFamily = Ui.uiFont)
    let enabledBox = CheckBox(Content = I18n.t "conn.use", FontFamily = Ui.uiFont)
    let kindBox =
        ComboBox(
            ItemsSource = (PlcLinkKind.all |> List.map kindLabel |> List.toArray),
            HorizontalAlignment = HorizontalAlignment.Stretch,
            FontFamily = Ui.uiFont
        )

    let ipBox = TextBox(FontFamily = Ui.monoFont)
    let portBox = NumericUpDown(Minimum = 1m, Maximum = 65535m, Increment = 1m, FormatString = "0", FontFamily = Ui.monoFont)

    let serialBox = TextBox(FontFamily = Ui.monoFont)
    let detectedBox =
        ComboBox(
            ItemsSource = (SerialBusRegistry.availablePorts ()),
            HorizontalAlignment = HorizontalAlignment.Stretch,
            FontFamily = Ui.monoFont,
            PlaceholderText = I18n.t "conn.detected"
        )
    let baudBox =
        ComboBox(
            ItemsSource = (Limits.bauds |> List.map string |> List.toArray),
            HorizontalAlignment = HorizontalAlignment.Stretch,
            FontFamily = Ui.monoFont
        )
    let dataBitsBox =
        ComboBox(ItemsSource = [| "7"; "8" |], HorizontalAlignment = HorizontalAlignment.Stretch, FontFamily = Ui.monoFont)
    let parityBox =
        ComboBox(
            ItemsSource = (Limits.parities |> List.map parityLabel |> List.toArray),
            HorizontalAlignment = HorizontalAlignment.Stretch,
            FontFamily = Ui.uiFont
        )
    let stopBitsBox =
        ComboBox(ItemsSource = [| "1"; "2" |], HorizontalAlignment = HorizontalAlignment.Stretch, FontFamily = Ui.monoFont)
    let stationBox =
        NumericUpDown(
            Minimum = 0m,
            Maximum = decimal Limits.maxStation,
            Increment = 1m,
            FormatString = "0",
            FontFamily = Ui.monoFont
        )
    let cycleBox =
        NumericUpDown(
            Minimum = decimal Limits.minCycleMs,
            Maximum = decimal Limits.maxCycleMs,
            Increment = 50m,
            FormatString = "0",
            FontFamily = Ui.monoFont
        )

    let ethernetFields =
        Ui.stackV 0.0 [ Ui.field (I18n.t "conn.ip") ipBox :> Control; Ui.field (I18n.t "conn.port") portBox :> Control ]

    let serialFields =
        Ui.stackV 0.0 [
            Ui.field (I18n.t "conn.serialPort") serialBox :> Control
            Ui.field (I18n.t "conn.detected") detectedBox :> Control
            Ui.field (I18n.t "conn.baud") baudBox :> Control
            Ui.field (I18n.t "conn.dataBits") dataBitsBox :> Control
            Ui.field (I18n.t "conn.parity") parityBox :> Control
            Ui.field (I18n.t "conn.stopBits") stopBitsBox :> Control
            Ui.field (I18n.t "conn.station") stationBox :> Control
        ]

    let current () = if index >= 0 && index < items.Count then Some items.[index] else None

    let refreshList () =
        let keep = index
        list.ItemsSource <- items |> Seq.map rowLabel |> Seq.toArray
        list.SelectedIndex <- (if keep >= 0 && keep < items.Count then keep else (if items.Count = 0 then -1 else 0))

    /// 지금 고른 PLC 를 고친다. 목록 줄도 함께 따라간다.
    /// 여기서는 값을 다듬지 않는다. 글자를 칠 때마다 다듬으면 이름 가운데 공백이 사라진다.
    /// (다듬기는 [확인] 을 누를 때 한 번에 한다)
    let update (f: PlcLink -> PlcLink) =
        if not suppress then
            match current () with
            | Some link ->
                items.[index] <- f link
                let keep = index
                suppress <- true
                refreshList ()
                index <- keep
                list.SelectedIndex <- keep
                suppress <- false
            | None -> ()

    let applyKindVisibility (kind: PlcLinkKind) =
        ethernetFields.IsVisible <- (kind = LinkEthernet)
        serialFields.IsVisible <- kind.IsSerial

    let load () =
        suppress <- true
        match current () with
        | Some l ->
            nameBox.Text <- l.Name
            enabledBox.IsChecked <- l.Enabled
            kindBox.SelectedIndex <- (PlcLinkKind.all |> List.findIndex (fun k -> k = l.Kind))
            ipBox.Text <- l.Ip
            portBox.Value <- decimal l.Port
            serialBox.Text <- l.SerialPort
            baudBox.SelectedIndex <-
                (match Limits.bauds |> List.tryFindIndex (fun b -> b = l.Baud) with
                 | Some i -> i
                 | None -> Limits.bauds |> List.findIndex (fun b -> b = Limits.defaultBaud))
            dataBitsBox.SelectedIndex <- (if l.DataBits = 7 then 0 else 1)
            parityBox.SelectedIndex <-
                (match Limits.parities |> List.tryFindIndex (fun x -> x = l.Parity) with
                 | Some i -> i
                 | None -> 0)
            stopBitsBox.SelectedIndex <- (if l.StopBits >= 2 then 1 else 0)
            stationBox.Value <- decimal l.Station
            cycleBox.Value <- decimal l.CycleMs
            applyKindVisibility l.Kind
        | None -> ()
        suppress <- false

    list.SelectionChanged.Add(fun _ ->
        if not suppress && list.SelectedIndex >= 0 then
            index <- list.SelectedIndex
            load ())

    nameBox.TextChanged.Add(fun _ -> update (fun l -> { l with Name = nameBox.Text }))
    enabledBox.IsCheckedChanged.Add(fun _ ->
        update (fun l -> { l with Enabled = enabledBox.IsChecked.HasValue && enabledBox.IsChecked.Value }))
    kindBox.SelectionChanged.Add(fun _ ->
        if kindBox.SelectedIndex >= 0 then
            let kind = PlcLinkKind.all.[kindBox.SelectedIndex]
            update (fun l -> { l with Kind = kind })
            applyKindVisibility kind)
    ipBox.TextChanged.Add(fun _ -> update (fun l -> { l with Ip = ipBox.Text }))
    portBox.ValueChanged.Add(fun _ -> if portBox.Value.HasValue then update (fun l -> { l with Port = int portBox.Value.Value }))
    serialBox.TextChanged.Add(fun _ -> update (fun l -> { l with SerialPort = serialBox.Text }))
    detectedBox.SelectionChanged.Add(fun _ ->
        // 검색된 포트를 고르면 위 칸을 채워 준다. (직접 입력도 그대로 된다)
        match detectedBox.SelectedItem with
        | :? string as port when not (String.IsNullOrWhiteSpace port) ->
            if serialBox.Text <> port then serialBox.Text <- port
        | _ -> ())
    baudBox.SelectionChanged.Add(fun _ ->
        if baudBox.SelectedIndex >= 0 then update (fun l -> { l with Baud = Limits.bauds.[baudBox.SelectedIndex] }))
    dataBitsBox.SelectionChanged.Add(fun _ ->
        if dataBitsBox.SelectedIndex >= 0 then update (fun l -> { l with DataBits = (if dataBitsBox.SelectedIndex = 0 then 7 else 8) }))
    parityBox.SelectionChanged.Add(fun _ ->
        if parityBox.SelectedIndex >= 0 then update (fun l -> { l with Parity = Limits.parities.[parityBox.SelectedIndex] }))
    stopBitsBox.SelectionChanged.Add(fun _ ->
        if stopBitsBox.SelectedIndex >= 0 then update (fun l -> { l with StopBits = stopBitsBox.SelectedIndex + 1 }))
    stationBox.ValueChanged.Add(fun _ ->
        if stationBox.Value.HasValue then update (fun l -> { l with Station = int stationBox.Value.Value }))
    cycleBox.ValueChanged.Add(fun _ ->
        if cycleBox.Value.HasValue then update (fun l -> { l with CycleMs = int cycleBox.Value.Value }))

    // ---------- 목록 버튼 ----------
    let hint =
        let t = TextBlock(Text = I18n.t "dlg.plc.hint", TextWrapping = TextWrapping.Wrap, FontFamily = Ui.uiFont, MaxWidth = 520.0)
        t.FontSize <- 11.5
        t.Foreground <- Ui.brush p.TextMuted
        t :> Control

    let addButton =
        Ui.button (I18n.t "dlg.plc.add") [] (fun () ->
            let id = PlcLink.nextId items
            // 새 PLC 는 지금 고른 것과 같은 방식으로 만든다. (같은 회선에 국번만 늘리는 경우가 많다)
            let template =
                match current () with
                | Some l when l.Kind.IsSerial -> { l with Id = id; Name = id; Station = min Limits.maxStation (l.Station + 1) }
                | Some l -> { l with Id = id; Name = id }
                | None -> PlcLink.ethernet id
            items.Add { template with Enabled = true }
            index <- items.Count - 1
            refreshList ()
            load ())

    let removeButton =
        Ui.button (I18n.t "dlg.plc.remove") [ "danger" ] (fun () ->
            if items.Count <= 1 then
                Dialogs.info owner (I18n.t "dlg.plc.title") (I18n.t "dlg.plc.last") |> ignore
            else
                items.RemoveAt index
                index <- max 0 (index - 1)
                refreshList ()
                load ())

    let buttonBar =
        StackPanel(
            Orientation = Orientation.Horizontal,
            Spacing = 8.0,
            HorizontalAlignment = HorizontalAlignment.Right,
            Margin = Thickness(0.0, 16.0, 0.0, 0.0)
        )

    let body =
        Ui.stackV 8.0 [
            hint
            list :> Control
            Ui.stackH 6.0 [ addButton :> Control; removeButton :> Control ] :> Control
            Ui.hSep ()
            Ui.field (I18n.t "conn.name") nameBox :> Control
            enabledBox :> Control
            Ui.field (I18n.t "conn.kind") kindBox :> Control
            ethernetFields :> Control
            serialFields :> Control
            Ui.field (I18n.t "conn.cycle") cycleBox :> Control
            buttonBar :> Control
        ]

    let win = Dialogs.panelWindow owner (I18n.t "dlg.plc.title") body 560.0

    let ok =
        Ui.button (I18n.t "btn.ok") [ "primary" ] (fun () ->
            let result = items |> Seq.map PlcLink.normalize |> List.ofSeq
            match Project.validatePlcs result with
            | Error message -> Dialogs.error owner message |> ignore
            | Ok() ->
                tcs.TrySetResult(Some result) |> ignore
                win.Close())
    ok.MinWidth <- 88.0
    let cancel =
        Ui.button (I18n.t "btn.cancel") [] (fun () ->
            tcs.TrySetResult None |> ignore
            win.Close())
    cancel.MinWidth <- 88.0
    buttonBar.Children.Add ok
    buttonBar.Children.Add cancel

    refreshList ()
    load ()

    win.Closed.Add(fun _ -> tcs.TrySetResult None |> ignore)
    win.ShowDialog owner |> ignore
    win, tcs.Task

/// PLC 목록을 고치고 [확인] 을 누르면 새 목록을, [취소] 면 None 을 돌려준다.
let edit (owner: Window) (existing: PlcLink list) : Task<PlcLink list option> =
    snd (editWindow owner existing)
