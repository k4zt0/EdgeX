/// 왼쪽 프로젝트 트리. XG5000 의 프로젝트 창과 같은 역할.
namespace XgbHmi.App.Views

open System
open System.Collections.Generic
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

[<AllowNullLiteral>]
type ProjectTreeView(state: AppState) =

    let tree = TreeView(FontFamily = Ui.uiFont, Padding = Thickness(4.0))
    let nodes = Dictionary<string, TreeViewItem>()
    let mutable syncing = false
    let subscriptions = ResizeArray<IDisposable>()

    let leafHeader (vm: ElementVm) =
        let p = ThemeService.current ()
        let dot =
            Border(
                Width = 8.0,
                Height = 8.0,
                CornerRadius = CornerRadius 4.0,
                VerticalAlignment = VerticalAlignment.Center,
                Background =
                    Ui.brush (
                        match vm.Kind with
                        | Switch -> p.KindSwitch
                        | Lamp -> p.KindLamp
                        | NumInput
                        | NumDisplay -> p.KindNumeric
                        | Text -> p.KindText
                    )
            )
        let name = Ui.text (if String.IsNullOrWhiteSpace vm.Name then "(no name)" else vm.Name)
        name.FontSize <- 12.5
        if not vm.Enabled then name.Opacity <- 0.45
        let device = Ui.mono 10.5 vm.Device
        device.Foreground <- Ui.brush p.TextMuted
        Ui.stackH 7.0 [ dot; name; device ] :> Control

    let groupHeader (caption: string) (count: int) =
        let p = ThemeService.current ()
        let t = Ui.text caption
        t.FontWeight <- FontWeight.SemiBold
        t.FontSize <- 12.5
        let c = Ui.text (sprintf "(%d)" count)
        c.FontSize <- 11.0
        c.Foreground <- Ui.brush p.TextMuted
        Ui.stackH 6.0 [ t; c ] :> Control

    let build () =
        nodes.Clear()
        tree.Items.Clear()

        let p = ThemeService.current ()
        let root = TreeViewItem(IsExpanded = true)
        let rootTitle = Ui.text (I18n.t "tree.project")
        rootTitle.FontWeight <- FontWeight.Bold
        root.Header <- rootTitle

        let connection = TreeViewItem(IsExpanded = false)
        let connText = Ui.text (sprintf "%s  —  %s:%d" (I18n.t "tree.connection") state.PlcIp state.Port)
        connText.FontSize <- 12.5
        connection.Header <- connText
        root.Items.Add connection |> ignore

        let screen = TreeViewItem(IsExpanded = true)
        screen.Header <- Ui.text (I18n.t "tree.screen")
        root.Items.Add screen |> ignore

        let groups =
            [ I18n.t "tree.group.switch", [ Switch ]
              I18n.t "tree.group.lamp", [ Lamp ]
              I18n.t "tree.group.numeric", [ NumInput; NumDisplay ]
              I18n.t "tree.group.text", [ Text ] ]

        for (caption, kinds) in groups do
            let members = state.Elements |> Seq.filter (fun e -> List.contains e.Kind kinds) |> List.ofSeq
            let node = TreeViewItem(IsExpanded = true)
            node.Header <- groupHeader caption members.Length
            for vm in members do
                let leaf = TreeViewItem(Header = leafHeader vm, Tag = vm.Id)
                nodes.[vm.Id] <- leaf
                node.Items.Add leaf |> ignore
            screen.Items.Add node |> ignore

        tree.Items.Add root |> ignore

    do
        build ()

        tree.SelectionChanged.Add(fun _ ->
            if not syncing then
                match tree.SelectedItem with
                | :? TreeViewItem as item when (item.Tag :? string) ->
                    match state.FindById(item.Tag :?> string) with
                    | Some vm ->
                        syncing <- true
                        state.Select(Some vm, false)
                        syncing <- false
                    | None -> ()
                | _ -> ())

        subscriptions.Add(state.StructureChanged.Subscribe(fun () -> build ()))

        subscriptions.Add(
            state.ItemChanged.Subscribe(fun (vm, prop) ->
                match prop with
                | "Name"
                | "Device"
                | "Enabled" ->
                    match nodes.TryGetValue vm.Id with
                    | true, node -> node.Header <- leafHeader vm
                    | _ -> ()
                | "Kind" -> build ()
                | _ -> ()))

        subscriptions.Add(
            state.SelectionChanged.Subscribe(fun () ->
                if not syncing then
                    syncing <- true
                    match state.Primary with
                    | Some vm ->
                        match nodes.TryGetValue vm.Id with
                        | true, node ->
                            node.IsSelected <- true
                            node.BringIntoView()
                        | _ -> ()
                    | None -> ()
                    syncing <- false))

    interface IDisposable with
        member _.Dispose() =
            for s in subscriptions do
                s.Dispose()
            subscriptions.Clear()

    member _.Root: Control = tree :> Control
    member _.Rebuild() = build ()
