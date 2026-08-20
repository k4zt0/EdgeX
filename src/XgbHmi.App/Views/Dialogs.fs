/// 메시지 상자 / 수량 입력 / 파일 선택. WinForms MessageBox 를 대신하며 3개 OS에서 같게 동작한다.
module XgbHmi.App.Views.Dialogs

open System
open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Platform.Storage
open XgbHmi.Core
open XgbHmi.App.Services

let private shell (owner: Window) (caption: string) (body: Control) (width: float) =
    let p = ThemeService.current ()
    let win =
        Window(
            Title = caption,
            Width = width,
            SizeToContent = SizeToContent.Height,
            CanResize = false,
            WindowStartupLocation = WindowStartupLocation.CenterOwner,
            Background = Ui.brush p.Surface,
            FontFamily = Ui.uiFont,
            ShowInTaskbar = false
        )
    win.FlowDirection <- (if I18n.isRtl () then FlowDirection.RightToLeft else FlowDirection.LeftToRight)

    let header =
        let t = Ui.title 14.0 caption
        Border(
            Background = Ui.brush p.Header,
            BorderBrush = Ui.brush p.Border,
            BorderThickness = Thickness(0.0, 0.0, 0.0, 1.0),
            Padding = Thickness(16.0, 10.0),
            Child = t
        )

    let root = DockPanel(LastChildFill = true)
    DockPanel.SetDock(header, Dock.Top)
    root.Children.Add header
    root.Children.Add(Border(Padding = Thickness(16.0), Child = body))
    win.Content <- root
    win

/// 같은 모양의 대화상자 창을 다른 화면(PLC 설정 등)에서도 쓴다.
let panelWindow (owner: Window) (caption: string) (body: Control) (width: float) = shell owner caption body width

let private messageIcon (kind: string) =
    let p = ThemeService.current ()
    let glyph, color =
        match kind with
        | "error" -> "!", p.Error
        | "confirm" -> "?", p.Warn
        | _ -> "i", p.Accent
    let t = Ui.title 18.0 glyph
    t.Foreground <- Ui.brush color
    t.HorizontalAlignment <- HorizontalAlignment.Center
    Border(
        Width = 34.0,
        Height = 34.0,
        CornerRadius = CornerRadius 17.0,
        Background = Ui.tint color 0.16,
        Child = t,
        VerticalAlignment = VerticalAlignment.Top
    )

/// 버튼 목록을 주면 눌린 버튼의 값을 돌려준다.
let private showButtons (owner: Window) (kind: string) (caption: string) (message: string) (buttons: (string * bool * bool) list) : Task<bool> =
    let tcs = TaskCompletionSource<bool>()
    let p = ThemeService.current ()

    let messageText = TextBlock(Text = message, TextWrapping = TextWrapping.Wrap, FontFamily = Ui.uiFont, MaxWidth = 420.0)
    messageText.Foreground <- Ui.brush p.Text

    let row = Ui.stackH 12.0 [ messageIcon kind; messageText ]
    row.VerticalAlignment <- VerticalAlignment.Top

    let buttonBar = StackPanel(Orientation = Orientation.Horizontal, Spacing = 8.0, HorizontalAlignment = HorizontalAlignment.Right, Margin = Thickness(0.0, 18.0, 0.0, 0.0))

    let body = Ui.stackV 0.0 [ row; buttonBar ]
    let win = shell owner caption body 470.0

    for (label, result, isPrimary) in buttons do
        let b = Ui.button label (if isPrimary then [ "primary" ] else []) (fun () ->
            tcs.TrySetResult result |> ignore
            win.Close())
        b.MinWidth <- 88.0
        buttonBar.Children.Add b

    win.Closed.Add(fun _ -> tcs.TrySetResult false |> ignore)
    win.ShowDialog owner |> ignore
    tcs.Task

let info (owner: Window) (caption: string) (message: string) =
    showButtons owner "info" caption message [ I18n.t "btn.ok", true, true ] :> Task

let error (owner: Window) (message: string) =
    showButtons owner "error" (I18n.t "msg.title.error") message [ I18n.t "btn.close", true, true ] :> Task

let confirm (owner: Window) (caption: string) (message: string) : Task<bool> =
    showButtons owner "confirm" caption message [ I18n.t "btn.yes", true, true; I18n.t "btn.no", false, false ]

/// 원본 PromptCount 대체: 1..max 범위의 수량 입력
let promptCount (owner: Window) (caption: string) (message: string) (defaultValue: int) (maxValue: int) : Task<int> =
    let tcs = TaskCompletionSource<int>()
    let p = ThemeService.current ()

    let label = TextBlock(Text = message, TextWrapping = TextWrapping.Wrap, FontFamily = Ui.uiFont)
    label.Foreground <- Ui.brush p.Text

    let numeric =
        NumericUpDown(
            Minimum = 1m,
            Maximum = decimal maxValue,
            Value = decimal (max 1 (min maxValue defaultValue)),
            Increment = 1m,
            FormatString = "0",
            Width = 140.0,
            HorizontalAlignment = HorizontalAlignment.Left
        )

    let buttonBar = StackPanel(Orientation = Orientation.Horizontal, Spacing = 8.0, HorizontalAlignment = HorizontalAlignment.Right, Margin = Thickness(0.0, 18.0, 0.0, 0.0))
    let body = Ui.stackV 10.0 [ label; Ui.field (I18n.t "dlg.count") numeric; buttonBar ]
    let win = shell owner caption body 420.0

    let ok =
        Ui.button (I18n.t "btn.ok") [ "primary" ] (fun () ->
            let v = if numeric.Value.HasValue then int numeric.Value.Value else 0
            tcs.TrySetResult v |> ignore
            win.Close())
    ok.MinWidth <- 88.0
    let cancel =
        Ui.button (I18n.t "btn.cancel") [] (fun () ->
            tcs.TrySetResult 0 |> ignore
            win.Close())
    cancel.MinWidth <- 88.0
    buttonBar.Children.Add ok
    buttonBar.Children.Add cancel

    win.Closed.Add(fun _ -> tcs.TrySetResult 0 |> ignore)
    win.ShowDialog owner |> ignore
    tcs.Task

let about (owner: Window) =
    let text = I18n.t "about.body" + "\n\n" + I18n.t "safety.banner"
    info owner (I18n.t "about.title") text

// ---------- 파일 선택 (OS 기본 대화상자) ----------

let private xmlFileType =
    FilePickerFileType(I18n.t "dlg.filter.xml", Patterns = [| "*.xml" |], MimeTypes = [| "application/xml"; "text/xml" |])

let openProjectFile (owner: Window) : Task<string option> =
    task {
        let options =
            FilePickerOpenOptions(
                Title = I18n.t "dlg.open.title",
                AllowMultiple = false,
                FileTypeFilter = [| xmlFileType |]
            )
        let! files = owner.StorageProvider.OpenFilePickerAsync options
        if files.Count = 0 then return None
        else
            let path = files.[0].TryGetLocalPath()
            return (if String.IsNullOrWhiteSpace path then None else Some path)
    }

let saveProjectFile (owner: Window) (suggestedPath: string) : Task<string option> =
    task {
        let suggestedName =
            if String.IsNullOrWhiteSpace suggestedPath then "hmi_project.xml"
            else Path.GetFileName suggestedPath
        let options =
            FilePickerSaveOptions(
                Title = I18n.t "dlg.save.title",
                SuggestedFileName = suggestedName,
                DefaultExtension = "xml",
                ShowOverwritePrompt = true,
                FileTypeChoices = [| xmlFileType |]
            )
        let! file = owner.StorageProvider.SaveFilePickerAsync options
        if isNull (box file) then return None
        else
            let path = file.TryGetLocalPath()
            return (if String.IsNullOrWhiteSpace path then None else Some path)
    }
