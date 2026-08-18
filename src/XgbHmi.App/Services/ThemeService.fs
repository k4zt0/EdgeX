namespace XgbHmi.App.Services

open System
open Avalonia
open Avalonia.Media
open Avalonia.Styling
open XgbHmi.App.Themes

/// 팔레트를 앱 리소스로 밀어 넣는다.
/// 모든 컨트롤은 DynamicResource 로 이 키들을 참조하므로 테마 전환이 즉시 반영된다.
[<RequireQualifiedAccess>]
module ThemeService =

    let private changedEvent = Event<Palette>()
    let changed = changedEvent.Publish

    let mutable private currentPalette = Palette.dark

    let current () = currentPalette

    let private setBrush (res: Controls.IResourceDictionary) (key: string) (hex: string) =
        res.[key] <- (SolidColorBrush(Color.Parse hex) :> IBrush)
        res.["Color." + key] <- Color.Parse hex

    /// 반투명 색 만들기 (선택 표시, 발광 등)
    let withAlpha (hex: string) (alpha: float) =
        let c = Color.Parse hex
        Color.FromArgb(byte (alpha * 255.0), c.R, c.G, c.B)

    let apply (p: Palette) =
        match Application.Current with
        | null -> ()
        | app ->
            let res = app.Resources
            setBrush res "App.Window" p.Window
            setBrush res "App.Surface" p.Surface
            setBrush res "App.SurfaceAlt" p.SurfaceAlt
            setBrush res "App.Header" p.Header
            setBrush res "App.StatusBar" p.StatusBar
            setBrush res "App.Border" p.Border
            setBrush res "App.BorderStrong" p.BorderStrong
            setBrush res "App.Text" p.Text
            setBrush res "App.TextMuted" p.TextMuted
            setBrush res "App.TextInverse" p.TextInverse
            setBrush res "App.Accent" p.Accent
            setBrush res "App.AccentHover" p.AccentHover
            setBrush res "App.AccentText" p.AccentText
            setBrush res "App.Selection" p.Selection
            setBrush res "App.CanvasBg" p.CanvasBg
            setBrush res "App.CanvasGrid" p.CanvasGrid
            setBrush res "App.CardBg" p.CardBg
            setBrush res "App.CardHeader" p.CardHeader
            setBrush res "App.CardBorder" p.CardBorder
            setBrush res "App.On" p.On
            setBrush res "App.Off" p.Off
            setBrush res "App.Ok" p.Ok
            setBrush res "App.Warn" p.Warn
            setBrush res "App.Error" p.Error
            setBrush res "App.Kind.Switch" p.KindSwitch
            setBrush res "App.Kind.Lamp" p.KindLamp
            setBrush res "App.Kind.Numeric" p.KindNumeric
            setBrush res "App.Kind.Text" p.KindText
            setBrush res "App.Glow" p.Glow

            // 마우스 오버 / 눌림 상태용 반투명 오버레이
            res.["App.Hover"] <- (SolidColorBrush(withAlpha p.Accent 0.16) :> IBrush)
            res.["App.Pressed"] <- (SolidColorBrush(withAlpha p.Accent 0.28) :> IBrush)
            res.["App.SelectionSoft"] <- (SolidColorBrush(withAlpha p.Accent 0.12) :> IBrush)

            // 내장 컨트롤(스크롤바, 콤보 팝업 등)이 테마와 맞도록 Fluent 변형도 함께 바꾼다.
            app.RequestedThemeVariant <- (if p.IsDark then ThemeVariant.Dark else ThemeVariant.Light)

            currentPalette <- p
            changedEvent.Trigger p

    let applyCode (code: string) = apply (Palette.byCode code)
