/// 오른쪽 속성 창. GT Designer / XP Builder 의 오브젝트 속성 창에 해당한다.
namespace XgbHmi.App.Views

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

[<AllowNullLiteral>]
type PropertyPanelView(state: AppState) =

    let mutable suppress = false
    let subscriptions = ResizeArray<IDisposable>()
    let mutable target: ElementVm option = None

    let emptyHint =
        let t = TextBlock(Text = I18n.t "prop.none", TextWrapping = TextWrapping.Wrap, FontFamily = Ui.uiFont, Margin = Thickness(12.0))
        t.Foreground <- Ui.brush (ThemeService.current ()).TextMuted
        t

    let enabledBox = CheckBox(Content = I18n.t "prop.enabled", FontFamily = Ui.uiFont)
    let visibleBox = CheckBox(Content = I18n.t "prop.visible", FontFamily = Ui.uiFont)
    let kindBox = ComboBox(ItemsSource = (ItemKind.all |> List.map I18n.kindLabel |> List.toArray), HorizontalAlignment = HorizontalAlignment.Stretch, FontFamily = Ui.uiFont)
    let nameBox = TextBox(FontFamily = Ui.uiFont)
    /// 이 요소가 쓸 PLC. PLC 를 한 대만 쓰면 감춘다.
    let plcBox = ComboBox(HorizontalAlignment = HorizontalAlignment.Stretch, FontFamily = Ui.uiFont)
    let deviceBox = TextBox(FontFamily = Ui.monoFont)
    let monitorBox = TextBox(FontFamily = Ui.monoFont)
    let actionBox = ComboBox(ItemsSource = (SwitchAction.all |> List.map I18n.actionLabel |> List.toArray), HorizontalAlignment = HorizontalAlignment.Stretch, FontFamily = Ui.uiFont)

    let numeric (minimum: int) (maximum: int) =
        NumericUpDown(
            Minimum = decimal minimum,
            Maximum = decimal maximum,
            Increment = 1m,
            FormatString = "0",
            FontFamily = Ui.monoFont,
            HorizontalAlignment = HorizontalAlignment.Stretch
        )

    let minBox = numeric -32768 65535
    let maxBox = numeric -32768 65535
    let xBox = numeric 0 100000
    let yBox = numeric 0 100000
    let wBox = numeric Limits.minWidth 100000
    let hBox = numeric Limits.minHeight 100000

    let sectionTitle (caption: string) =
        let t = Ui.text caption
        t.FontSize <- 11.0
        t.FontWeight <- FontWeight.Bold
        t.Foreground <- Ui.brush (ThemeService.current ()).TextMuted
        t.Margin <- Thickness(0.0, 12.0, 0.0, 4.0)
        t :> Control

    let plcField = Ui.field (I18n.t "prop.plc") plcBox
    let monitorField = Ui.field (I18n.t "prop.monitor") monitorBox
    let actionField = Ui.field (I18n.t "prop.action") actionBox
    let minField = Ui.field (I18n.t "prop.min") minBox
    let maxField = Ui.field (I18n.t "prop.max") maxBox

    let form =
        Ui.stackV 0.0 [
            sectionTitle (I18n.t "prop.section.general")
            enabledBox :> Control
            visibleBox :> Control
            Ui.field (I18n.t "prop.type") kindBox :> Control
            Ui.field (I18n.t "prop.name") nameBox :> Control
            sectionTitle (I18n.t "prop.section.device")
            plcField :> Control
            Ui.field (I18n.t "prop.device") deviceBox :> Control
            monitorField :> Control
            actionField :> Control
            minField :> Control
            maxField :> Control
            sectionTitle (I18n.t "prop.section.geometry")
            Ui.field (I18n.t "prop.x") xBox :> Control
            Ui.field (I18n.t "prop.y") yBox :> Control
            Ui.field (I18n.t "prop.width") wBox :> Control
            Ui.field (I18n.t "prop.height") hBox :> Control
        ]

    let container = Border(Padding = Thickness(12.0, 4.0, 12.0, 12.0), Child = emptyHint)
    let scroller = ScrollViewer(Content = container, VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto)

    let numValue (n: NumericUpDown) = if n.Value.HasValue then int n.Value.Value else 0

    /// 종류에 따라 쓰지 않는 칸은 감춘다.
    let applyKindVisibility (vm: ElementVm) =
        let isSwitch = ItemKind.hasAction vm.Kind
        let isBit = ItemKind.isBit vm.Kind
        let isWord = ItemKind.isWord vm.Kind
        monitorField.IsVisible <- isBit
        actionField.IsVisible <- isSwitch
        minField.IsVisible <- isWord
        maxField.IsVisible <- isWord
        // 텍스트와 통합 스위치는 제 주소를 쓰지 않는다.
        deviceBox.IsEnabled <- vm.Kind <> Text && vm.Kind <> MasterSwitch

    /// PLC 목록이 바뀌면 콤보 상자도 따라간다.
    let loadPlcs () =
        let plcs = state.Plcs
        plcBox.ItemsSource <- plcs |> List.map PlcLink.label |> List.toArray
        // 한 대만 쓰면 고를 것이 없으니 칸을 감춘다.
        plcField.IsVisible <- plcs.Length > 1

    let load () =
        suppress <- true
        loadPlcs ()
        match state.Primary with
        | Some vm ->
            target <- Some vm
            container.Child <- form
            plcBox.SelectedIndex <-
                (let id = state.PlcIdOf vm
                 match state.Plcs |> List.tryFindIndex (fun l -> String.Equals(l.Id, id, StringComparison.OrdinalIgnoreCase)) with
                 | Some i -> i
                 | None -> -1)
            enabledBox.IsChecked <- vm.Enabled
            visibleBox.IsChecked <- vm.Visible
            kindBox.SelectedIndex <- vm.KindIndex
            nameBox.Text <- vm.Name
            deviceBox.Text <- vm.Device
            monitorBox.Text <- vm.MonitorDevice
            actionBox.SelectedIndex <- vm.ActionIndex
            minBox.Value <- decimal vm.Min
            maxBox.Value <- decimal vm.Max
            xBox.Value <- decimal vm.X
            yBox.Value <- decimal vm.Y
            wBox.Value <- decimal vm.Width
            hBox.Value <- decimal vm.Height
            applyKindVisibility vm
        | None ->
            target <- None
            container.Child <- emptyHint
        suppress <- false

    let edit (f: ElementVm -> unit) =
        if not suppress then
            match target with
            | Some vm -> f vm
            | None -> ()

    do
        enabledBox.IsCheckedChanged.Add(fun _ -> edit (fun vm -> vm.Enabled <- enabledBox.IsChecked.HasValue && enabledBox.IsChecked.Value))
        visibleBox.IsCheckedChanged.Add(fun _ -> edit (fun vm -> vm.Visible <- visibleBox.IsChecked.HasValue && visibleBox.IsChecked.Value))
        kindBox.SelectionChanged.Add(fun _ -> edit (fun vm ->
            if kindBox.SelectedIndex >= 0 then
                vm.KindIndex <- kindBox.SelectedIndex
                applyKindVisibility vm))
        nameBox.TextChanged.Add(fun _ -> edit (fun vm -> vm.Name <- nameBox.Text))
        plcBox.SelectionChanged.Add(fun _ ->
            edit (fun vm ->
                let plcs = state.Plcs
                if plcBox.SelectedIndex >= 0 && plcBox.SelectedIndex < plcs.Length then
                    vm.PlcId <- plcs.[plcBox.SelectedIndex].Id))
        deviceBox.LostFocus.Add(fun _ -> edit (fun vm -> vm.Device <- deviceBox.Text))
        monitorBox.LostFocus.Add(fun _ -> edit (fun vm -> vm.MonitorDevice <- monitorBox.Text))
        actionBox.SelectionChanged.Add(fun _ -> edit (fun vm -> if actionBox.SelectedIndex >= 0 then vm.ActionIndex <- actionBox.SelectedIndex))
        minBox.ValueChanged.Add(fun _ -> edit (fun vm -> vm.Min <- numValue minBox))
        maxBox.ValueChanged.Add(fun _ -> edit (fun vm -> vm.Max <- numValue maxBox))
        xBox.ValueChanged.Add(fun _ -> edit (fun vm -> vm.X <- numValue xBox))
        yBox.ValueChanged.Add(fun _ -> edit (fun vm -> vm.Y <- numValue yBox))
        wBox.ValueChanged.Add(fun _ -> edit (fun vm -> vm.Width <- numValue wBox))
        hBox.ValueChanged.Add(fun _ -> edit (fun vm -> vm.Height <- numValue hBox))

        subscriptions.Add(state.SelectionChanged.Subscribe(fun () -> load ()))
        subscriptions.Add(state.PlcsChanged.Subscribe(fun () -> load ()))

        // 캔버스에서 끌어 옮기면 X/Y/W/H 칸도 즉시 따라온다.
        subscriptions.Add(
            state.ItemChanged.Subscribe(fun (vm, prop) ->
                match target with
                | Some current when Object.ReferenceEquals(current, vm) && not suppress ->
                    suppress <- true
                    (match prop with
                     | "X" -> xBox.Value <- decimal vm.X
                     | "Y" -> yBox.Value <- decimal vm.Y
                     | "Width" -> wBox.Value <- decimal vm.Width
                     | "Height" -> hBox.Value <- decimal vm.Height
                     | "Name" -> (if nameBox.Text <> vm.Name then nameBox.Text <- vm.Name)
                     | "Device" -> (if deviceBox.Text <> vm.Device then deviceBox.Text <- vm.Device)
                     | "Enabled" -> enabledBox.IsChecked <- vm.Enabled
                     | "Visible" -> visibleBox.IsChecked <- vm.Visible
                     | "Kind" ->
                         kindBox.SelectedIndex <- vm.KindIndex
                         applyKindVisibility vm
                     | _ -> ())
                    suppress <- false
                | _ -> ()))

        load ()

    interface IDisposable with
        member _.Dispose() =
            for s in subscriptions do
                s.Dispose()
            subscriptions.Clear()

    member _.Root: Control = scroller :> Control
    member _.Reload() = load ()
