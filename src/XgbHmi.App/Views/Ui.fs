/// 코드로 화면을 만들 때 반복되는 조각들.
module XgbHmi.App.Views.Ui

open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.App.Themes

let inline private addClasses (c: #StyledElement) (classes: string list) =
    for cls in classes do
        c.Classes.Add cls
    c

let brush (hex: string) : IBrush = SolidColorBrush(Color.Parse hex) :> IBrush

let color (hex: string) = Color.Parse hex

/// 반투명 브러시 (선택 강조, 배경 틴트 등)
let tint (hex: string) (alpha: float) : IBrush =
    let c = Color.Parse hex
    SolidColorBrush(Color.FromArgb(byte (alpha * 255.0), c.R, c.G, c.B)) :> IBrush

/// 화면에서 쓰는 글꼴. CJK / 태국어 / 아랍어 / 데바나가리까지 대체 글꼴을 나열한다.
let uiFont =
    FontFamily.Parse(
        "Inter, Segoe UI, SF Pro Text, Helvetica Neue, Malgun Gothic, Apple SD Gothic Neo, Noto Sans KR, "
        + "Noto Sans CJK KR, Hiragino Sans, Yu Gothic UI, Noto Sans JP, Microsoft YaHei, Noto Sans SC, "
        + "Microsoft JhengHei, Noto Sans TC, Noto Sans Thai, Noto Sans Devanagari, Noto Sans Arabic, sans-serif"
    )

/// 값 표시용 고정폭 글꼴
let monoFont =
    FontFamily.Parse("JetBrains Mono, Cascadia Mono, SF Mono, Menlo, Consolas, DejaVu Sans Mono, monospace")

let text (s: string) =
    TextBlock(Text = s, VerticalAlignment = VerticalAlignment.Center, FontFamily = uiFont)

let textCls (classes: string list) (s: string) = addClasses (text s) classes

let muted (s: string) = textCls [ "muted" ] s

let title (size: float) (s: string) =
    TextBlock(Text = s, FontSize = size, FontWeight = FontWeight.SemiBold, VerticalAlignment = VerticalAlignment.Center, FontFamily = uiFont)

let mono (size: float) (s: string) =
    TextBlock(Text = s, FontSize = size, FontFamily = monoFont, VerticalAlignment = VerticalAlignment.Center)

let stackH (spacing: float) (children: Control seq) =
    let p = StackPanel(Orientation = Orientation.Horizontal, Spacing = spacing, VerticalAlignment = VerticalAlignment.Center)
    for c in children do
        p.Children.Add c
    p

let stackV (spacing: float) (children: Control seq) =
    let p = StackPanel(Orientation = Orientation.Vertical, Spacing = spacing)
    for c in children do
        p.Children.Add c
    p

let button (label: string) (classes: string list) (onClick: unit -> unit) =
    let b = Button(Content = label, FontFamily = uiFont)
    b.Click.Add(fun _ -> onClick ())
    addClasses b classes

let toolButton (label: string) (tip: string) (onClick: unit -> unit) =
    let b = button label [ "tool" ] onClick
    if not (System.String.IsNullOrWhiteSpace tip) then ToolTip.SetTip(b, tip)
    b

let toggleButton (label: string) (classes: string list) (isChecked: bool) (onChange: bool -> unit) =
    let t = ToggleButton(Content = label, IsChecked = isChecked, FontFamily = uiFont)
    t.IsCheckedChanged.Add(fun _ -> onChange (t.IsChecked.HasValue && t.IsChecked.Value))
    addClasses t classes

/// 세로 구분선 (툴바용)
let vSep () =
    let p = Palette.byId (XgbHmi.App.Services.ThemeService.current ()).Id
    Border(Width = 1.0, Margin = Thickness(6.0, 5.0, 6.0, 5.0), Background = brush p.Border) :> Control

let hSep () =
    let p = XgbHmi.App.Services.ThemeService.current ()
    Border(Height = 1.0, Background = brush p.Border) :> Control

let spacer () =
    let b = Border()
    b.HorizontalAlignment <- HorizontalAlignment.Stretch
    b :> Control

/// 둥근 모서리 패널
let panel (padding: Thickness) (child: Control) =
    let p = XgbHmi.App.Services.ThemeService.current ()
    Border(
        Background = brush p.Surface,
        BorderBrush = brush p.Border,
        BorderThickness = Thickness 1.0,
        CornerRadius = CornerRadius 6.0,
        Padding = padding,
        Child = child
    )

/// 도킹 패널 제목 표시줄 (프로젝트 / 속성 / 출력)
let panelHeader (caption: string) (extra: Control option) =
    let p = XgbHmi.App.Services.ThemeService.current ()
    let dock = DockPanel(LastChildFill = true, Height = 30.0)
    match extra with
    | Some e ->
        DockPanel.SetDock(e, Dock.Right)
        dock.Children.Add e
    | None -> ()
    let caption = title 12.0 caption
    caption.Foreground <- brush p.TextMuted
    caption.Margin <- Thickness(10.0, 0.0, 0.0, 0.0)
    dock.Children.Add caption
    Border(Background = brush p.Header, BorderBrush = brush p.Border, BorderThickness = Thickness(0.0, 0.0, 0.0, 1.0), Child = dock)

/// 상태 표시 알약 (연결 상태, 쓰기 잠금 등)
let pill (fill: string) (fg: string) (caption: string) =
    let t = text caption
    t.Foreground <- brush fg
    t.FontSize <- 11.5
    t.FontWeight <- FontWeight.SemiBold
    Border(
        Background = brush fill,
        CornerRadius = CornerRadius 10.0,
        Padding = Thickness(9.0, 2.0, 9.0, 3.0),
        Child = t,
        VerticalAlignment = VerticalAlignment.Center
    )

let scroll (child: Control) =
    ScrollViewer(
        Content = child,
        HorizontalScrollBarVisibility = Avalonia.Controls.Primitives.ScrollBarVisibility.Auto,
        VerticalScrollBarVisibility = Avalonia.Controls.Primitives.ScrollBarVisibility.Auto
    )

let dockTop (c: Control) =
    DockPanel.SetDock(c, Dock.Top)
    c

let dockBottom (c: Control) =
    DockPanel.SetDock(c, Dock.Bottom)
    c

let dockLeft (c: Control) =
    DockPanel.SetDock(c, Dock.Left)
    c

let dockRight (c: Control) =
    DockPanel.SetDock(c, Dock.Right)
    c

let dock (children: Control seq) =
    let d = DockPanel(LastChildFill = true)
    for c in children do
        d.Children.Add c
    d

/// 라벨 + 입력칸 한 줄 (속성 창)
let field (labelText: string) (editor: Control) =
    let g = Grid()
    g.ColumnDefinitions.Add(ColumnDefinition(GridLength(108.0, GridUnitType.Pixel)))
    g.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
    g.Margin <- Thickness(0.0, 2.0, 0.0, 2.0)
    let l = muted labelText
    l.FontSize <- 12.0
    l.VerticalAlignment <- VerticalAlignment.Center
    Grid.SetColumn(l, 0)
    Grid.SetColumn(editor, 1)
    g.Children.Add l
    g.Children.Add editor
    g

let menuItem (header: string) (onClick: unit -> unit) =
    let mi = MenuItem(Header = header, FontFamily = uiFont)
    mi.Click.Add(fun _ -> onClick ())
    mi

let checkableMenuItem (header: string) (isChecked: bool) (onClick: unit -> unit) =
    let mi = MenuItem(Header = header, ToggleType = MenuItemToggleType.CheckBox, IsChecked = isChecked, FontFamily = uiFont)
    mi.Click.Add(fun _ -> onClick ())
    mi

let radioMenuItem (header: string) (isChecked: bool) (onClick: unit -> unit) =
    let mi = MenuItem(Header = header, ToggleType = MenuItemToggleType.Radio, IsChecked = isChecked, FontFamily = uiFont)
    mi.Click.Add(fun _ -> onClick ())
    mi

let separatorItem () = Separator() :> Control
