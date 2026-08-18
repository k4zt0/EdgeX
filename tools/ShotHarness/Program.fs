module ShotHarness.Program

open System
open System.IO
open Avalonia
open Avalonia.Headless
open Avalonia.Media.Imaging
open Avalonia.Threading
open XgbHmi.App.Services

/// 헤드리스로 창을 렌더해 PNG 로 저장한다 (UI 확인용, 배포에는 포함되지 않음).
[<EntryPoint>]
let main argv =
    let dragMode = argv |> Array.contains "--drag"
    let argv = argv |> Array.filter (fun a -> a <> "--drag")
    let outDir = if argv.Length > 0 then argv.[0] else "./shots"
    Directory.CreateDirectory outDir |> ignore

    let builder =
        AppBuilder
            .Configure<XgbHmi.App.App>()
            .UseSkia()
            .WithInterFont()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))

    builder.SetupWithoutStarting() |> ignore

    let settings = { AppSettings.defaults with WindowWidth = 1600.0; WindowHeight = 1000.0 }

    let shoot (themeCode: string) (langCode: string) (name: string) =
        ThemeService.applyCode themeCode
        XgbHmi.Core.I18n.setLanguage langCode
        let window, _ = XgbHmi.App.Views.MainWindow.createWithRebuild settings
        window.Show()
        Dispatcher.UIThread.RunJobs()
        let frame = HeadlessWindowExtensions.CaptureRenderedFrame window
        match frame with
        | null -> printfn "capture failed: %s" name
        | bmp ->
            let path = Path.Combine(outDir, name + ".png")
            bmp.Save path
            printfn "saved %s" path
        window.Close()

    if dragMode then
        ThemeService.applyCode "dark"
        XgbHmi.Core.I18n.setLanguage "ko"
        let window, _ = XgbHmi.App.Views.MainWindow.createWithRebuild settings
        window.Show()
        DragProbe.run window
        window.Close()
        exit 0

    let targets =
        if argv.Length > 1 then
            argv.[1..] |> Array.map (fun spec ->
                let parts = spec.Split ':'
                parts.[0], parts.[1])
        else
            [| "dark", "ko"; "cyberpunk", "en"; "light", "ja"; "xg5000", "ko"; "blueprint", "ru"; "contrast", "ar" |]

    for (theme, lang) in targets do
        shoot theme lang (theme + "-" + lang)

    // 테마/언어를 여러 번 바꿔도 화면이 정상적으로 다시 만들어지는지 확인한다.
    ThemeService.applyCode "dark"
    XgbHmi.Core.I18n.setLanguage "ko"
    let window, rebuild = XgbHmi.App.Views.MainWindow.createWithRebuild settings
    window.Show()
    Dispatcher.UIThread.RunJobs()

    for (themeCode, langCode) in [ "light", "en"; "cyberpunk", "ja"; "contrast", "ar"; "xg5000", "zh-Hans"; "blueprint", "hi"; "dark", "ko" ] do
        ThemeService.applyCode themeCode
        XgbHmi.Core.I18n.setLanguage langCode
        rebuild ()
        Dispatcher.UIThread.RunJobs()

    match HeadlessWindowExtensions.CaptureRenderedFrame window with
    | null -> printfn "switch check: capture failed"
    | bmp ->
        let path = Path.Combine(outDir, "after-switching.png")
        bmp.Save path
        printfn "switch check OK -> %s" path
    window.Close()
    0
