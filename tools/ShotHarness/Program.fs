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

    // 이 도구는 창을 여닫으며 화면만 찍는다. 사용자의 settings.json 을 건드리면 안 된다.
    AppSettings.setReadOnly true

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

    // PLC 통신 설정 창을 찍는다. (--plc 또는 --plc:테마:언어)
    let plcMode = argv |> Array.tryPick (fun a -> if a = "--plc" then Some "dark:ko" elif a.StartsWith "--plc:" then Some(a.Substring 6) else None)
    let argv = argv |> Array.filter (fun a -> a <> "--plc" && not (a.StartsWith "--plc:"))
    match plcMode with
    | Some spec ->
        let parts = spec.Split ':'
        ThemeService.applyCode parts.[0]
        XgbHmi.Core.I18n.setLanguage parts.[1]
        let window, _ = XgbHmi.App.Views.MainWindow.createWithRebuild settings
        window.Show()
        Dispatcher.UIThread.RunJobs()
        // 이더넷 한 대 + 같은 RS-485 회선에 국번이 다른 두 대를 넣고 창을 띄운다.
        let plcs =
            [ { XgbHmi.Core.PlcLink.ethernet "PLC1" with Name = "1호기 반송" }
              { XgbHmi.Core.PlcLink.serial "PLC2" XgbHmi.Core.LinkRs485 1 with
                  Name = "2호기 세정"
                  SerialPort = "COM3" }
              { XgbHmi.Core.PlcLink.serial "PLC3" XgbHmi.Core.LinkRs232 0 with
                  Name = "3호기 검사"
                  SerialPort = "COM4" } ]
        let dialog, _ = XgbHmi.App.Views.PlcDialog.editWindow window plcs
        Dispatcher.UIThread.RunJobs()
        let shot (name: string) =
            match HeadlessWindowExtensions.CaptureRenderedFrame dialog with
            | null -> printfn "capture failed: %s" name
            | bmp ->
                let path = Path.Combine(outDir, sprintf "%s-%s-%s.png" name parts.[0] parts.[1])
                bmp.Save path
                printfn "saved %s" path
        // 이더넷 PLC 를 고른 모습과 직렬(RS-485) PLC 를 고른 모습을 둘 다 찍는다.
        shot "plc-ethernet"
        DragProbe.descendants dialog
        |> Seq.tryPick (fun v -> match v with :? ListBox as l -> Some l | _ -> None)
        |> Option.iter (fun l -> l.SelectedIndex <- 1)
        Dispatcher.UIThread.RunJobs()
        shot "plc-serial"
        dialog.Close()
        window.Close()
        exit 0
    | None -> ()

    // 실제 통신까지 확인하는 모드. XGBHMI_AUTOCONNECT=1 과 함께 쓰면
    // 창을 띄운 뒤 잠시 돌려 폴링 값이 화면에 들어온 모습을 찍는다. (--live 또는 --live:3000)
    let pressMode = argv |> Array.contains "--press"
    /// PLC 별 HMI 창을 전부 띄워 본다. (여러 대를 동시에 조작하는 모습 확인)
    let multiMode = argv |> Array.contains "--multi"
    let argv = argv |> Array.filter (fun a -> a <> "--press" && a <> "--multi")
    let liveMode =
        argv
        |> Array.tryPick (fun a ->
            if a = "--live" then Some 3000
            elif a.StartsWith "--live:" then Some(int (a.Substring 7))
            else None)
    let argv = argv |> Array.filter (fun a -> a <> "--live" && not (a.StartsWith "--live:"))
    match liveMode with
    | Some durationMs ->
        let theme, lang =
            if argv.Length > 1 then
                let parts = argv.[1].Split ':'
                parts.[0], parts.[1]
            else "dark", "ko"
        ThemeService.applyCode theme
        XgbHmi.Core.I18n.setLanguage lang
        let window, _ = XgbHmi.App.Views.MainWindow.createWithRebuild settings
        window.Show()
        let sw = Diagnostics.Stopwatch.StartNew()
        while sw.ElapsedMilliseconds < int64 durationMs do
            Dispatcher.UIThread.RunJobs()
            Threading.Thread.Sleep 50
        Dispatcher.UIThread.RunJobs()
        let shotWindow (name: string) (w: Window) =
            match HeadlessWindowExtensions.CaptureRenderedFrame w with
            | null -> printfn "capture failed: %s" name
            | bmp ->
                let path = Path.Combine(outDir, sprintf "%s-%s-%s.png" name theme lang)
                bmp.Save path
                printfn "saved %s" path
        shotWindow "live" window
        // 운전 중에는 터치 패널이 따로 열린다. 그 창도 찍는다. (위 띠에 PLC 목록이 나온다)
        match XgbHmi.App.Views.MainWindow.hmiWindowForTools with
        | null -> printfn "HMI 창이 열려 있지 않다"
        | hmiWin ->
            shotWindow "live-hmiwin" hmiWin
            // 조작 시험은 마지막으로 띄운 창에서 한다. (--multi 면 PLC 별 창)
            let mutable pressTarget = hmiWin
            // --multi 면 보기 메뉴로 PLC 별 창을 하나씩 더 띄운다.
            if multiMode then
                let rec findMenuItem (items: Collections.IEnumerable) (header: string) : MenuItem option =
                    let mutable found = None
                    for o in items do
                        if found.IsNone then
                            match o with
                            | :? MenuItem as mi ->
                                match mi.Header with
                                | :? string as h when h = header -> found <- Some mi
                                | _ ->
                                    match findMenuItem mi.Items header with
                                    | Some x -> found <- Some x
                                    | None -> ()
                            | _ -> ()
                    found

                let click (mi: MenuItem) =
                    mi.RaiseEvent(Avalonia.Interactivity.RoutedEventArgs(MenuItem.ClickEvent))
                    Dispatcher.UIThread.RunJobs()
                    Threading.Thread.Sleep 200
                    Dispatcher.UIThread.RunJobs()

                let menus =
                    DragProbe.descendants window
                    |> Seq.tryPick (fun v -> match v with :? Menu as m -> Some m | _ -> None)
                match menus |> Option.bind (fun m -> findMenuItem m.Items (XgbHmi.Core.I18n.t "hmi.window")) with
                | None -> printfn "보기 메뉴에서 HMI 창 항목을 찾지 못했다"
                | Some hmiMenu ->
                    let perPlc =
                        hmiMenu.Items
                        |> Seq.cast<obj>
                        |> Seq.choose (fun o -> match o with :? MenuItem as mi -> Some mi | _ -> None)
                        |> Seq.filter (fun mi ->
                            match mi.Header with
                            | :? string as h -> h <> XgbHmi.Core.I18n.t "hmi.window.all"
                            | _ -> false)
                        |> List.ofSeq
                    printfn "PLC 별 HMI 창 %d개를 띄운다" perPlc.Length
                    perPlc
                    |> List.iteri (fun i mi ->
                        click mi
                        match XgbHmi.App.Views.MainWindow.hmiWindowForTools with
                        | null -> printfn "  %d: 창이 열리지 않았다" (i + 1)
                        | w ->
                            printfn "  %d: %s" (i + 1) w.Title
                            pressTarget <- w
                            shotWindow (sprintf "live-hmiwin-plc%d" (i + 1)) w)

            // --press 면 실제로 버튼을 하나 눌러 본다. (어느 PLC 를 제어 중인지 뜨는지 확인)
            if pressMode then
                let pump (ms: int) =
                    let sw = Diagnostics.Stopwatch.StartNew()
                    while sw.ElapsedMilliseconds < int64 ms do
                        Dispatcher.UIThread.RunJobs()
                        Threading.Thread.Sleep 20
                    Dispatcher.UIThread.RunJobs()

                let clickCenter (w: Window) (target: Visual) =
                    let bounds = target.Bounds
                    match target.TranslatePoint(Point(bounds.Width / 2.0, bounds.Height / 2.0), w) with
                    | v when v.HasValue ->
                        w.MouseDown(v.Value, Avalonia.Input.MouseButton.Left)
                        Dispatcher.UIThread.RunJobs()
                        w.MouseUp(v.Value, Avalonia.Input.MouseButton.Left)
                        pump 120
                        true
                    | _ -> false

                // 1) 쓰기 허용을 켠다. 확인 대화상자가 뜨므로 [예] 를 눌러 준다.
                let writeToggle =
                    DragProbe.toggleButtons window
                    |> List.tryFind (fun t ->
                        match t.Content with
                        | :? string as c -> c = XgbHmi.Core.I18n.t "cmd.writeEnable"
                        | _ -> false)
                if writeToggle.IsNone then printfn "툴바에서 쓰기 허용 토글을 찾지 못했다"
                writeToggle
                |> Option.iter (fun t ->
                    t.IsChecked <- true
                    pump 150
                    match window.OwnedWindows |> Seq.tryHead with
                    | Some dialog ->
                        let yes =
                            DragProbe.descendants dialog
                            |> Seq.tryPick (fun v ->
                                match v with
                                | :? Button as b when (match b.Content with
                                                       | :? string as c -> c = XgbHmi.Core.I18n.t "btn.yes"
                                                       | _ -> false) -> Some b
                                | _ -> None)
                        if yes.IsNone then printfn "확인 대화상자에서 [예] 를 찾지 못했다"
                        yes |> Option.iter (fun b -> clickCenter dialog b |> ignore)
                        pump 150
                    | None -> printfn "쓰기 허용 확인 대화상자를 찾지 못했다")

                // 2) 터치 패널의 부품 하나를 누른다.
                // 위 띠에 '제어 중 / 마지막 조작' 이 떴는지 읽는다.
                let controlText () =
                    DragProbe.descendants pressTarget
                    |> Seq.tryPick (fun v ->
                        match v with
                        | :? TextBlock as t when
                            not (String.IsNullOrWhiteSpace t.Text)
                            && (t.Text.StartsWith(XgbHmi.Core.I18n.t "hmi.controlling")
                                || t.Text.StartsWith(XgbHmi.Core.I18n.t "hmi.lastControl")) -> Some t.Text
                        | _ -> None)

                // 글자 부품 같은 것은 눌러도 명령이 나가지 않는다. 조작이 나갈 때까지 몇 개 눌러 본다.
                let buttons =
                    DragProbe.cardBorders pressTarget
                    |> List.filter (fun b -> b.Bounds.Width <= 200.0 && b.Bounds.Height <= 200.0)
                let mutable pressed = false
                for part in buttons do
                    if not pressed then
                        clickCenter pressTarget part |> ignore
                        pump 400
                        match controlText () with
                        | Some text ->
                            pressed <- true
                            printfn "부품을 눌렀다: [%s] %s  ->  %s" pressTarget.Title (DragProbe.describe part) text
                            shotWindow "live-hmiwin-press" pressTarget
                        | None -> ()
                if not pressed then printfn "눌러도 조작이 나가지 않았다"
            hmiWin.Close()
        window.Close()
        exit 0
    | None -> ()

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
