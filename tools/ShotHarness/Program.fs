module ShotHarness.Program

open System
open System.IO
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Media.Imaging
open Avalonia.Threading
open XgbHmi.App.Services

/// 헤드리스로 창을 렌더해 PNG 로 저장한다 (UI 확인용, 배포에는 포함되지 않음).
[<EntryPoint>]
let main argv =
    let dragMode = argv |> Array.contains "--drag"
    /// 특정 문서 탭을 골라 찍는다. `--hmi` 는 마지막 탭(HMI), `--tab:1` 처럼 번호도 쓸 수 있다.
    let hmiMode = argv |> Array.exists (fun a -> a = "--hmi" || a.StartsWith "--tab:")
    let wantedTab =
        argv
        |> Array.tryPick (fun a -> if a.StartsWith "--tab:" then Some(int (a.Substring 6)) else None)
    let argv = argv |> Array.filter (fun a -> a <> "--drag" && a <> "--hmi" && not (a.StartsWith "--tab:"))
    let outDir = if argv.Length > 0 then argv.[0] else "./shots"
    Directory.CreateDirectory outDir |> ignore

    let builder =
        AppBuilder
            .Configure<XgbHmi.App.App>()
            .UseSkia()
            .WithInterFont()
            .UseHeadless(AvaloniaHeadlessPlatformOptions(UseHeadlessDrawing = false))

    builder.SetupWithoutStarting() |> ignore

    // 이 도구는 창을 여닫으며 화면만 찍는다. 사용자의 settings.json 을 건드리면 안 된다.
    AppSettings.setReadOnly true

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

    let gaugeProbe = argv |> Array.tryPick (fun a -> if a.StartsWith "--gauge:" then Some(a.Substring 8) else None)
    let argv = argv |> Array.filter (fun a -> not (a.StartsWith "--gauge:"))

    match gaugeProbe with
    | Some gaugeId ->
        ThemeService.applyCode "dark"
        XgbHmi.Core.I18n.setLanguage "ko"
        let window, _ = XgbHmi.App.Views.MainWindow.createWithRebuild settings
        window.Show()
        DragProbe.probeGauge window gaugeId
        window.Close()
        exit 0
    | None -> ()

    if hmiMode then
        let targets =
            if argv.Length > 1 then
                argv.[1..] |> Array.map (fun spec ->
                    let parts = spec.Split ':'
                    parts.[0], parts.[1])
            else [| "dark", "ko" |]

        for (themeCode, langCode) in targets do
            ThemeService.applyCode themeCode
            XgbHmi.Core.I18n.setLanguage langCode
            let window, _ = XgbHmi.App.Views.MainWindow.createWithRebuild settings
            window.Show()
            Dispatcher.UIThread.RunJobs()
            // 문서 탭 중 마지막(HMI)을 고른다.
            DragProbe.descendants window
            |> Seq.tryPick (fun v ->
                match v with
                | :? TabControl as t when t.ItemCount >= 3 -> Some t
                | _ -> None)
            |> Option.iter (fun tabs ->
                tabs.SelectedIndex <- defaultArg wantedTab (tabs.ItemCount - 1))
            Dispatcher.UIThread.RunJobs()
            // 부품 하나를 눌러 골라 둔다. 속성창이 채워진 모습까지 확인하려는 것.
            match (if wantedTab.IsSome then [] else DragProbe.cardBorders window) with
            | [] -> printfn "HMI 부품이 없다"
            | parts ->
                let part = parts |> List.maxBy (fun b -> Avalonia.Controls.Canvas.GetTop b)
                match part.TranslatePoint(Point(part.Bounds.Width / 2.0, part.Bounds.Height / 2.0), window) with
                | v when v.HasValue ->
                    window.MouseDown(v.Value, Avalonia.Input.MouseButton.Left)
                    Dispatcher.UIThread.RunJobs()
                    window.MouseUp(v.Value, Avalonia.Input.MouseButton.Left)
                    Dispatcher.UIThread.RunJobs()
                | _ -> printfn "부품 좌표를 창 좌표로 바꾸지 못했다"
            Dispatcher.UIThread.RunJobs()
            match HeadlessWindowExtensions.CaptureRenderedFrame window with
            | null -> printfn "capture failed: hmi-%s-%s" themeCode langCode
            | bmp ->
                let path = Path.Combine(outDir, sprintf "tab%d-%s-%s.png" (defaultArg wantedTab 99) themeCode langCode)
                bmp.Save path
                printfn "saved %s" path
            window.Close()
        exit 0

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
