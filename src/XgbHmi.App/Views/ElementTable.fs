/// 화면 편집 표. XG5000 의 파라미터 표처럼 여러 행을 한 번에 다룬다.
namespace XgbHmi.App.Views

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Data
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

/// 표에서 바로 눌러 보는 조작
type TableCommands =
    { /// 그 요소의 스위치 동작을 실제로 수행한다.
      Run: ElementVm -> unit
      /// 직전 실행 전의 값으로 되돌린다.
      Revert: ElementVm -> unit }

[<AllowNullLiteral>]
type ElementTableView(state: AppState, commands: TableCommands) =

    /// PLC 를 여러 대 쓸 때만 보여 주는 칸
    let mutable plcColumn: DataGridColumn = null

    let grid =
        DataGrid(
            AutoGenerateColumns = false,
            CanUserReorderColumns = true,
            CanUserResizeColumns = true,
            CanUserSortColumns = false,
            SelectionMode = DataGridSelectionMode.Extended,
            HeadersVisibility = DataGridHeadersVisibility.Column,
            GridLinesVisibility = DataGridGridLinesVisibility.All,
            FontFamily = Ui.uiFont,
            ItemsSource = state.Elements
        )

    let mutable syncing = false
    let subscriptions = ResizeArray<IDisposable>()

    let twoWay (path: string) : BindingBase =
        Binding(Path = path, Mode = BindingMode.TwoWay) :> BindingBase

    let textColumn (header: string) (path: string) (width: float) =
        let c = DataGridTextColumn(Header = header, Binding = twoWay path, Width = DataGridLength width)
        c.FontFamily <- Ui.uiFont
        c

    let comboColumn (header: string) (path: string) (options: string list) (width: float) =
        let template =
            FuncDataTemplate<ElementVm>(
                (fun vm _ ->
                    let combo =
                        ComboBox(
                            ItemsSource = List.toArray options,
                            Margin = Thickness(2.0, 1.0),
                            FontFamily = Ui.uiFont,
                            HorizontalAlignment = HorizontalAlignment.Stretch,
                            MinHeight = 26.0
                        )
                    combo.Bind(ComboBox.SelectedIndexProperty, twoWay path) |> ignore
                    combo :> Control),
                true
            )
        DataGridTemplateColumn(Header = header, CellTemplate = template, Width = DataGridLength width)

    /// 어느 PLC 를 쓸지 고르는 칸. 목록은 셀을 그릴 때마다 지금 PLC 목록에서 가져온다.
    let plcSelectColumn (header: string) (width: float) =
        let template =
            FuncDataTemplate<ElementVm>(
                (fun vm _ ->
                    let combo =
                        ComboBox(
                            ItemsSource = (state.Plcs |> List.map (fun l -> l.Id) |> List.toArray),
                            Margin = Thickness(2.0, 1.0),
                            FontFamily = Ui.monoFont,
                            HorizontalAlignment = HorizontalAlignment.Stretch,
                            MinHeight = 26.0
                        )
                    combo.Bind(ComboBox.SelectedItemProperty, twoWay "PlcId") |> ignore
                    combo :> Control),
                true
            )
        DataGridTemplateColumn(Header = header, CellTemplate = template, Width = DataGridLength width)

    let checkColumn (header: string) (path: string) (width: float) =
        DataGridCheckBoxColumn(Header = header, Binding = twoWay path, Width = DataGridLength width)

    /// 행마다 '실행 / 실행 취소' 버튼을 둔다.
    /// 행이 재사용되므로 누른 시점의 DataContext 를 보고 대상을 정한다.
    let commandColumn (header: string) (width: float) =
        let make (label: string) (tip: string) (action: ElementVm -> unit) =
            let b = Ui.button label [ "hmi" ] (fun () -> ())
            b.MinHeight <- 22.0
            b.FontSize <- 11.0
            b.Padding <- Thickness(6.0, 0.0)
            ToolTip.SetTip(b, tip)
            b.Click.Add(fun _ ->
                match b.DataContext with
                | :? ElementVm as vm -> action vm
                | _ -> ())
            b

        let template =
            FuncDataTemplate<ElementVm>(
                (fun vm _ ->
                    let runButton = make (I18n.t "cmd.runNow") (I18n.t "cmd.runNow") commands.Run
                    let revertButton = make (I18n.t "cmd.revertRun") (I18n.t "cmd.revertRunTip") commands.Revert
                    // 조작 버튼이 있는 종류에서만 누를 수 있다.
                    let usable = not (isNull (box vm)) && ItemKind.hasAction vm.Kind
                    runButton.IsEnabled <- usable
                    revertButton.IsEnabled <- usable
                    let row = Ui.stackH 3.0 [ runButton; revertButton ]
                    row.Margin <- Thickness(2.0, 1.0)
                    row :> Control),
                true
            )
        DataGridTemplateColumn(Header = header, CellTemplate = template, Width = DataGridLength width)

    do
        grid.Columns.Add(checkColumn (I18n.t "prop.enabled") "Enabled" 56.0)
        // '운전 화면 표시' 는 표에서 빼고 속성 창에만 둔다. 표는 주소와 동작에 집중한다.
        grid.Columns.Add(comboColumn (I18n.t "prop.type") "KindIndex" (ItemKind.all |> List.map I18n.kindLabel) 132.0)
        grid.Columns.Add(textColumn (I18n.t "prop.name") "Name" 190.0)
        plcColumn <- plcSelectColumn (I18n.t "prop.plc") 92.0
        grid.Columns.Add plcColumn
        grid.Columns.Add(textColumn (I18n.t "prop.device") "Device" 110.0)
        grid.Columns.Add(textColumn (I18n.t "prop.monitor") "MonitorDevice" 140.0)
        grid.Columns.Add(comboColumn (I18n.t "prop.action") "ActionIndex" (SwitchAction.all |> List.map I18n.actionLabel) 128.0)
        grid.Columns.Add(textColumn (I18n.t "prop.min") "Min" 78.0)
        grid.Columns.Add(textColumn (I18n.t "prop.max") "Max" 78.0)
        grid.Columns.Add(textColumn (I18n.t "prop.x") "X" 62.0)
        grid.Columns.Add(textColumn (I18n.t "prop.y") "Y" 62.0)
        grid.Columns.Add(textColumn (I18n.t "prop.width") "Width" 70.0)
        grid.Columns.Add(textColumn (I18n.t "prop.height") "Height" 70.0)
        grid.Columns.Add(commandColumn (I18n.t "cmd.runNow") 128.0)
        plcColumn.IsVisible <- state.Plcs.Length > 1

        grid.SelectionChanged.Add(fun _ ->
            if not syncing then
                syncing <- true
                let selected = grid.SelectedItems |> Seq.cast<obj> |> Seq.choose (fun o -> match o with :? ElementVm as v -> Some v | _ -> None)
                state.SelectMany selected
                syncing <- false)

        subscriptions.Add(
            state.SelectionChanged.Subscribe(fun () ->
                if not syncing then
                    syncing <- true
                    grid.SelectedItems.Clear()
                    for vm in state.Selection do
                        grid.SelectedItems.Add vm |> ignore
                    match state.Primary with
                    | Some vm -> grid.ScrollIntoView(vm, null)
                    | None -> ()
                    syncing <- false))

    interface IDisposable with
        member _.Dispose() =
            for s in subscriptions do
                s.Dispose()
            subscriptions.Clear()

    /// PLC 목록이 바뀌면 PLC 칸을 보이거나 감춘다. (한 대만 쓰면 감춘다)
    member _.RefreshPlcColumn() =
        if not (isNull plcColumn) then plcColumn.IsVisible <- state.Plcs.Length > 1

    member _.Root: Control = grid :> Control
    member _.Grid = grid

    /// 우클릭 메뉴를 바깥(메인 창)에서 붙인다.
    member _.SetContextMenu(menu: ContextMenu) = grid.ContextMenu <- menu
