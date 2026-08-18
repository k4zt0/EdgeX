/// 아래쪽 출력 창(통신 로그). XG5000 의 '출력 창'에 해당한다.
/// 시각 · 등급 · 내용 3단으로 보여 주고, TX/RX 원문까지 그대로 남길 수 있다.
namespace XgbHmi.App.Views

open System
open System.Collections.ObjectModel
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Templates
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.App.Services

type LogLine(time: string, level: LogLevel, tag: string, message: string) =
    member _.Time = time
    member _.Level = level
    member _.Tag = tag
    member _.Message = message

[<AllowNullLiteral>]
type OutputPanelView() =

    let lines = ObservableCollection<LogLine>()
    let maxLines = 4000

    let levelTag (level: LogLevel) =
        match level with
        | Success -> "OK "
        | Warn -> "WARN"
        | Failure -> "ERR "
        | Trace -> "TRC "
        | Info -> "INFO"

    let levelColor (level: LogLevel) =
        let p = ThemeService.current ()
        match level with
        | Success -> p.Ok
        | Warn -> p.Warn
        | Failure -> p.Error
        | Trace -> p.TextMuted
        | Info -> p.Text

    let template =
        FuncDataTemplate<LogLine>(
            (fun line _ ->
                let p = ThemeService.current ()
                let color = levelColor line.Level

                let time = Ui.mono 11.0 line.Time
                time.Foreground <- Ui.brush p.TextMuted
                time.VerticalAlignment <- VerticalAlignment.Top

                let tag = Ui.mono 10.5 line.Tag
                tag.Foreground <- Ui.brush color
                tag.FontWeight <- (if line.Level = Trace then FontWeight.Normal else FontWeight.Bold)
                tag.VerticalAlignment <- VerticalAlignment.Top

                let text =
                    TextBlock(
                        Text = line.Message,
                        FontFamily = Ui.monoFont,
                        FontSize = 11.5,
                        TextWrapping = TextWrapping.Wrap,
                        Foreground = Ui.brush color
                    )

                let row = Grid(Margin = Thickness(10.0, 1.0, 10.0, 1.0))
                row.ColumnDefinitions.Add(ColumnDefinition(GridLength(88.0, GridUnitType.Pixel)))
                row.ColumnDefinitions.Add(ColumnDefinition(GridLength(38.0, GridUnitType.Pixel)))
                row.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
                Grid.SetColumn(time, 0)
                Grid.SetColumn(tag, 1)
                Grid.SetColumn(text, 2)
                row.Children.Add time
                row.Children.Add tag
                row.Children.Add text
                row :> Control),
            true
        )

    let list = ItemsControl(ItemsSource = lines, ItemTemplate = template)

    let scroller =
        ScrollViewer(
            Content = list,
            VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto,
            HorizontalScrollBarVisibility = Primitives.ScrollBarVisibility.Disabled
        )

    member val AutoScroll = true with get, set

    member _.Root: Control = scroller :> Control

    member _.Count = lines.Count

    member this.Append(level: LogLevel, message: string) =
        let stamp = DateTime.Now.ToString "HH:mm:ss.fff"
        let text = if isNull message then "" else message.TrimEnd()
        let parts = text.Split '\n'
        for i in 0 .. parts.Length - 1 do
            let body = parts.[i].TrimEnd('\r')
            if i = 0 then lines.Add(LogLine(stamp, level, levelTag level, body))
            else lines.Add(LogLine("", level, "", body))
        while lines.Count > maxLines do
            lines.RemoveAt 0
        if this.AutoScroll then scroller.ScrollToEnd()

    member _.Clear() = lines.Clear()
