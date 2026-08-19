/// XG5000 / XP Builder / GT Designer 계열 엔지니어링 툴 배치를 따른 메인 창.
/// 위: 메뉴 + 툴바 / 왼쪽: 프로젝트 트리 / 가운데: 문서 탭 / 오른쪽: 속성 / 아래: 출력 / 맨 아래: 상태 표시줄
module XgbHmi.App.Views.MainWindow

open System
open System.IO
open System.Threading.Tasks
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open Avalonia.Threading
open Avalonia.VisualTree
open XgbHmi.Core
open XgbHmi.Protocol
open XgbHmi.App.Themes
open XgbHmi.App.Services
open XgbHmi.App.ViewModels

type private LogRecord = { Level: LogLevel; Message: string }

/// 창과 함께 '화면 다시 만들기' 함수를 돌려준다. (테마/언어 전환 경로 검증용)
let createWithRebuild (initialSettings: AppSettings) : Window * (unit -> unit) =

    let state = AppState()
    let plc = new PlcService()
    let mutable settings = initialSettings

    let win =
        Window(
            Width = settings.WindowWidth,
            Height = settings.WindowHeight,
            MinWidth = 1024.0,
            MinHeight = 680.0,
            WindowStartupLocation = WindowStartupLocation.CenterScreen,
            FontFamily = Ui.uiFont
        )

    // ---------- 세션 상태 ----------
    let logHistory = ResizeArray<LogRecord>()
    let mutable output: OutputPanelView = null
    let mutable canvasView: RunCanvasView = null
    /// 운전 중에 따로 띄우는 모니터링 창과 그 캔버스
    let mutable monitorWindow: Window = null
    let mutable monitorCanvas: RunCanvasView = null
    let mutable monitorStatus: TextBlock = null
    let mutable canvasHost: CanvasHost option = None
    let mutable tableView: ElementTableView = null
    let mutable treeView: ProjectTreeView = null
    let mutable propertyView: PropertyPanelView = null

    let mutable layoutMode = false
    let mutable writeEnabled = false
    let mutable connected = false
    /// PLC 통신 전체가 오류 상태인지. 운전 화면의 모든 카드를 빨간색으로 점등하는 데 쓴다.
    let mutable commFault = false
    /// 지금 돌고 있는 조작들 (요소 Id 기준). 통합 스위치가 한 줄씩 보여 준다.
    /// 실패는 ElementVm.Fault 로 남으므로 여기에는 도는 것만 담는다.
    let runningOps = Collections.Generic.Dictionary<string, CardFactory.RunningOp>()
    let mutable showProjectPanel = true
    let mutable showPropertyPanel = true
    let mutable showOutputPanel = true

    let mutable statusPill: Border = null
    let mutable statusText: TextBlock = null
    let mutable statusProfile: TextBlock = null
    let mutable statusItems: TextBlock = null
    let mutable statusGeometry: TextBlock = null
    let mutable statusWrite: Border = null
    let mutable ipBox: TextBox = null
    let mutable portBox: NumericUpDown = null
    let mutable cycleBox: NumericUpDown = null
    let mutable connectButton: Button = null
    let mutable disconnectButton: Button = null
    let mutable writeToggle: ToggleButton = null
    let mutable zoomLabel: TextBlock = null
    let mutable layoutModeItem: MenuItem = null
    let mutable documentTabs: TabControl = null
    let mutable layoutHintText: TextBlock = null
    let mutable layoutHintBar: Border = null
    let mutable logStatsText: TextBlock = null
    let mutable titleText: TextBlock = null

    let mutable rebuildUi: unit -> unit = fun () -> ()
    let mutable suppressWriteToggle = false

    // ---------- 로그 ----------
    let log (level: LogLevel) (message: string) =
        logHistory.Add { Level = level; Message = message }
        while logHistory.Count > 2000 do
            logHistory.RemoveAt 0
        if not (isNull output) then output.Append(level, message)

    let onUi (f: unit -> unit) =
        if Dispatcher.UIThread.CheckAccess() then f () else Dispatcher.UIThread.Post(fun () -> f ())

    // ---------- 제목 / 상태 ----------
    let projectLabel () =
        let name =
            if String.IsNullOrWhiteSpace state.ProjectPath then "(unsaved)"
            else Path.GetFileName state.ProjectPath
        sprintf "%s%s — %s" name (if state.IsDirty then " *" else "") (I18n.t "app.title")

    let updateTitle () =
        win.Title <- projectLabel ()
        if not (isNull titleText) then titleText.Text <- projectLabel ()

    /// 출력 창 머리글의 통신 통계를 갱신한다.
    let updateLogStats () =
        if not (isNull logStatsText) then
            logStatsText.Text <-
                I18n.tf
                    "log.stats"
                    [| box plc.CycleCount
                       box plc.FrameCount
                       box plc.ErrorCount
                       box (int (System.Math.Round plc.LastCycleMs)) |]

    let updateItemCount () =
        if not (isNull statusItems) then
            let selected = state.SelectionCount
            statusItems.Text <-
                I18n.tf "status.items" [| box state.Elements.Count |]
                + (if selected > 0 then "   ·   " + I18n.tf "status.selected" [| box selected |] else "")

    let setStatus (kind: ConnState) (detail: string) =
        commFault <- (kind = Faulted)
        if not (isNull statusText) then
            let p = ThemeService.current ()
            let caption, fill, fg =
                match kind with
                | Disconnected -> I18n.t "status.disconnected", Ui.tint p.Off 0.25, p.TextMuted
                | Connecting -> I18n.t "status.connecting", Ui.tint p.Warn 0.25, p.Warn
                | Online -> I18n.t "status.online", Ui.tint p.Ok 0.22, p.Ok
                | Faulted -> I18n.t "status.error", Ui.tint p.Error 0.22, p.Error
            statusPill.Background <- fill
            match statusPill.Child with
            | :? TextBlock as t ->
                t.Text <- caption
                t.Foreground <- Ui.brush fg
            | _ -> ()
            statusText.Text <- detail
            statusText.Foreground <- Ui.brush (if kind = Faulted then p.Error else p.TextMuted)
            if not (isNull monitorStatus) then
                monitorStatus.Text <- caption + "   ·   " + detail
                monitorStatus.Foreground <- Ui.brush fg
            statusProfile.Text <-
                if String.IsNullOrWhiteSpace plc.ProfileName then ""
                else I18n.t "conn.profile" + ": " + plc.ProfileName

    let updateWriteBadge () =
        if not (isNull statusWrite) then
            let p = ThemeService.current ()
            match statusWrite.Child with
            | :? TextBlock as t ->
                t.Text <- (if writeEnabled then I18n.t "status.writeEnabled" else I18n.t "status.writeLocked")
                t.Foreground <- Ui.brush (if writeEnabled then p.TextInverse else p.TextMuted)
            | _ -> ()
            statusWrite.Background <- (if writeEnabled then Ui.brush p.Error else Ui.tint p.Off 0.22)

    /// 배치 편집이 켜졌는지에 따라 안내 문구를 바꾼다.
    let setLayoutHint (on: bool) =
        if not (isNull layoutHintText) then
            let p = ThemeService.current ()
            layoutHintText.Text <- (if on then I18n.t "canvas.hint" else I18n.t "canvas.hintOff")
            layoutHintText.Foreground <- Ui.brush (if on then p.Accent else p.TextMuted)
            if not (isNull layoutHintBar) then
                layoutHintBar.Background <- (if on then Ui.tint p.Accent 0.14 else Ui.tint p.Off 0.10)

    /// 배치 편집을 켜고 끈다. 툴바 메뉴와 F2 가 함께 쓴다.
    let setLayoutMode (on: bool) =
        layoutMode <- on
        if not (isNull canvasView) then canvasView.LayoutMode <- on
        if not (isNull layoutModeItem) then layoutModeItem.IsChecked <- on
        setLayoutHint on
        log Info ("LAYOUT MODE " + (if on then "ON" else "OFF"))

    // ---------- PLC 값 표시 ----------
    /// 지금 살아 있는 운전 화면 캔버스들. (문서 탭 + 모니터링 창)
    let canvases () =
        [ if not (isNull canvasView) then yield canvasView
          if not (isNull monitorCanvas) then yield monitorCanvas ]

    let rebuildCanvases () =
        for c in canvases () do
            c.Rebuild()

    let refreshValues () =
        let status: CardFactory.RuntimeStatus =
            { BitOf = (fun addr -> plc.TryBit addr)
              WordOf = (fun addr -> plc.TryWord addr)
              CommFault = commFault
              Operations = List.ofSeq runningOps.Values }
        for c in canvases () do
            c.RefreshValues status

    /// 조작을 시작했다고 알린다. (통합 스위치에 '실행 중' 으로 뜬다)
    let beginOp (vm: ElementVm) (action: string) =
        runningOps.[vm.Id] <-
            { Id = vm.Id
              Name = (if String.IsNullOrWhiteSpace vm.Name then vm.Device else vm.Name)
              Device = vm.Device
              Action = action
              Phase = CardFactory.OpRunning
              Message = "" }
        refreshValues ()

    /// 조작이 끝났다고 알린다. 실패는 vm.Fault 로 남아 계속 빨갛게 보인다.
    let endOp (vm: ElementVm) (_ok: bool) (_message: string) =
        runningOps.Remove vm.Id |> ignore
        refreshValues ()

    // ---------- 운전 화면 모니터링 창 ----------
    /// 운전 중에는 운전 화면을 따로 띄운다. 배치 편집은 하지 않고 보고 조작만 한다.
    let closeMonitorWindow () =
        if not (isNull monitorWindow) then
            let w = monitorWindow
            monitorWindow <- null
            w.Close()

    let openMonitorWindow () =
        match canvasHost with
        | None -> ()
        | Some host ->
            if isNull monitorWindow then
                let p = ThemeService.current ()

                let view = new RunCanvasView(state, host)
                view.ShowGrid <- false
                view.SnapToGrid <- false
                // 모니터링 창에서는 요소를 옮기지 않는다. 잘못 끌어 옮기는 사고를 막는다.
                view.LayoutMode <- false
                view.Rebuild()
                monitorCanvas <- view

                let statusText = Ui.mono 11.5 ""
                statusText.Foreground <- Ui.brush p.TextMuted
                statusText.Margin <- Thickness(10.0, 0.0, 10.0, 0.0)
                monitorStatus <- statusText

                let bar =
                    Border(
                        Background = Ui.brush p.StatusBar,
                        BorderBrush = Ui.brush p.Border,
                        BorderThickness = Thickness(0.0, 1.0, 0.0, 0.0),
                        Height = 26.0,
                        Child = statusText
                    )

                let root = DockPanel(LastChildFill = true)
                DockPanel.SetDock(bar, Dock.Bottom)
                root.Children.Add bar
                root.Children.Add view.Root

                let w =
                    Window(
                        Title = projectLabel () + " — " + I18n.t "tab.run",
                        Width = 1280.0,
                        Height = 820.0,
                        MinWidth = 480.0,
                        MinHeight = 320.0,
                        Background = Ui.brush p.Window,
                        FontFamily = Ui.uiFont,
                        WindowStartupLocation = WindowStartupLocation.CenterScreen,
                        Content = root
                    )
                w.FlowDirection <- (if I18n.isRtl () then FlowDirection.RightToLeft else FlowDirection.LeftToRight)
                w.Closed.Add(fun _ ->
                    (match box monitorCanvas with
                     | :? IDisposable as d -> d.Dispose()
                     | _ -> ())
                    monitorCanvas <- null
                    monitorStatus <- null
                    monitorWindow <- null
                    log Info "MONITOR WINDOW CLOSED")

                monitorWindow <- w
                w.Show()
                view.FitToWindow()
                refreshValues ()
                // 같은 화면을 두 군데서 보지 않도록 본 창은 화면 편집으로 넘긴다.
                if not (isNull documentTabs) then documentTabs.SelectedIndex <- 1
                log Info "MONITOR WINDOW OPEN"

    // ---------- 프로젝트 입출력 ----------
    let loadProjectFrom (path: string) =
        try
            let project = ProjectIo.load path
            state.LoadProject(project, path)
            if not (isNull ipBox) then ipBox.Text <- project.PlcIp
            if not (isNull portBox) then portBox.Value <- decimal project.Port
            if not (isNull cycleBox) then cycleBox.Value <- decimal project.CycleMs
            rebuildCanvases ()
            if not (isNull treeView) then treeView.Rebuild()
            updateTitle ()
            updateItemCount ()
            log Info ("PROJECT LOAD " + path)
            Ok()
        with ex -> Error ex.Message

    let saveProjectTo (path: string) =
        try
            if not (isNull ipBox) then state.PlcIp <- ipBox.Text.Trim()
            if not (isNull portBox) && portBox.Value.HasValue then state.Port <- int portBox.Value.Value
            if not (isNull cycleBox) && cycleBox.Value.HasValue then state.CycleMs <- int cycleBox.Value.Value
            ProjectIo.save path (state.ToProject())
            state.ProjectPath <- path
            state.MarkSaved()
            updateTitle ()
            log Success ("PROJECT SAVE " + path)
            Ok()
        with ex -> Error ex.Message

    // ---------- PLC 쓰기 가드 (원본 EnsureCanWrite) ----------
    let ensureCanWrite () : bool =
        if layoutMode then
            Dialogs.info win (I18n.t "cmd.layoutMode") (I18n.t "msg.layoutBlocked") |> ignore
            false
        elif not writeEnabled then
            Dialogs.info win (I18n.t "cmd.writeEnable") (I18n.t "msg.writeLocked") |> ignore
            false
        elif not (plc.IsRunning && connected) then
            Dialogs.info win (I18n.t "cmd.connect") (I18n.t "msg.notConnected") |> ignore
            false
        else true

    /// 조용한 확인용 (순간 스위치를 뗄 때 등, 대화상자를 띄우지 않는다)
    let canWriteSilently () = not layoutMode && writeEnabled && plc.IsRunning && connected

    /// 로그에 함께 남길 XGT 직접변수 이름 (예: M01008 -> %MX1608)
    let xgtName (device: string) =
        try
            let d = if isNull device then "" else device.Trim().ToUpperInvariant()
            if d.StartsWith "D" then Address.toXgtWord 'D' (Address.parseDWord "읽기" d)
            else Address.toXgtBit d
        with _ -> "?"

    let onOff (v: bool) = if v then "ON" else "OFF"

    let writeBit (vm: ElementVm) (value: bool) =
        let device = vm.Device
        let started = DateTime.Now
        beginOp vm (onOff value)
        Task.Run(fun () ->
            match plc.WriteBitVerified(device, value) with
            | Ok readback ->
                let elapsed = (DateTime.Now - started).TotalMilliseconds
                onUi (fun () ->
                    vm.Fault <- None
                    endOp vm true ""
                    let rbText =
                        match readback with
                        | Some rb -> sprintf "  READBACK=%s" (onOff rb)
                        | None -> "  READBACK=없음"
                    log Success (sprintf "BIT WRITE %s (%s) <- %s%s  %.0f ms  [%s]" device (xgtName device) (onOff value) rbText elapsed vm.Name)
                    match readback with
                    | Some rb when rb <> value ->
                        log Warn (
                            sprintf
                                "WARNING %s requested %s but PLC readback is %s. PLC ladder/other write may be overwriting this bit."
                                device
                                (if value then "ON" else "OFF")
                                (if rb then "ON" else "OFF")
                        )
                    | _ -> ())
            | Error message ->
                onUi (fun () ->
                    vm.Fault <- Some message
                    endOp vm false message
                    log Failure (sprintf "BIT WRITE ERROR %s (%s) <- %s  %.0f ms : %s" device (xgtName device) (onOff value) (DateTime.Now - started).TotalMilliseconds message)
                    Dialogs.error win (I18n.tf "msg.writeFailed" [| box device; box message |]) |> ignore))
        |> ignore

    /// 토글은 클릭 순간 PLC의 실제 상태를 읽어 반전한다. (v4 토글 수정과 동일)
    let toggleBit (vm: ElementVm) =
        let device = vm.Device
        let started = DateTime.Now
        beginOp vm (I18n.actionLabel Toggle)
        Task.Run(fun () ->
            match plc.ReadBitNow device with
            | Ok current ->
                onUi (fun () ->
                    log Info (sprintf "TOGGLE READ %s (%s) = %s  ->  WRITE %s  [%s]" device (xgtName device) (onOff current) (onOff (not current)) vm.Name)
                    refreshValues ())
                match plc.WriteBitVerified(device, not current) with
                | Ok readback ->
                    let elapsed = (DateTime.Now - started).TotalMilliseconds
                    onUi (fun () ->
                        vm.Fault <- None
                        endOp vm true ""
                        let rbText =
                            match readback with
                            | Some rb -> sprintf "  READBACK=%s" (onOff rb)
                            | None -> "  READBACK=없음"
                        log Success (sprintf "BIT WRITE %s (%s) <- %s%s  %.0f ms" device (xgtName device) (onOff (not current)) rbText elapsed)
                        match readback with
                        | Some rb when rb = current ->
                            log Warn (sprintf "WARNING %s readback did not change. PLC ladder may be overwriting this bit." device)
                        | _ -> ())
                | Error message ->
                    onUi (fun () ->
                        vm.Fault <- Some message
                        endOp vm false message
                        log Failure (sprintf "BIT WRITE ERROR %s (%s): %s" device (xgtName device) message)
                        Dialogs.error win (I18n.tf "msg.writeFailed" [| box device; box message |]) |> ignore)
            | Error message ->
                onUi (fun () ->
                    vm.Fault <- Some message
                    endOp vm false message
                    log Failure (sprintf "TOGGLE READ ERROR %s (%s): %s" device (xgtName device) message)
                    Dialogs.error win (I18n.tf "msg.toggleReadFailed" [| box device; box message |]) |> ignore))
        |> ignore

    let writeWord (vm: ElementVm) (entered: int) =
        let device = vm.Device
        let raw = uint16 (entered &&& 0xFFFF)
        let started = DateTime.Now
        beginOp vm (I18n.t "btn.write")
        Task.Run(fun () ->
            match plc.WriteWordVerified(device, raw) with
            | Ok readback ->
                let elapsed = (DateTime.Now - started).TotalMilliseconds
                onUi (fun () ->
                    vm.Fault <- None
                    endOp vm true ""
                    log Success (
                        sprintf
                            "WORD WRITE %s (%s) <- %d (0x%04X)  READBACK=%d (0x%04X, signed %d)  %.0f ms  [%s]"
                            device (xgtName device) entered raw readback readback (int16 readback) elapsed vm.Name))
            | Error message ->
                onUi (fun () ->
                    vm.Fault <- Some message
                    endOp vm false message
                    log Failure (sprintf "WORD WRITE ERROR %s (%s) <- %d: %s" device (xgtName device) entered message)
                    Dialogs.error win (I18n.tf "msg.writeFailed" [| box device; box message |]) |> ignore))
        |> ignore

    /// 킬 스위치. 조작할 수 있는 비트를 모두 OFF 로 쓴다.
    /// 확인 대화상자는 두지 않는다. 끄는 방향은 안전한 쪽이고, 급할 때 한 번에 눌러야 한다.
    /// (쓰기 자체는 'PLC 쓰기 허용' 을 켜야만 열린다)
    let killAll () =
        if ensureCanWrite () then
            let victims =
                state.Elements
                |> Seq.filter (fun e ->
                    e.Enabled && ItemKind.hasAction e.Kind && not (String.IsNullOrWhiteSpace e.Device))
                |> List.ofSeq
            log Warn (sprintf "KILL SWITCH — 비트 %d개를 OFF 로 씁니다" victims.Length)
            for vm in victims do
                writeBit vm false

    let cardCallbacks: CardFactory.CardCallbacks =
        { Toggle = fun vm -> if ensureCanWrite () then toggleBit vm
          WriteOn = fun vm -> if ensureCanWrite () then writeBit vm true
          WriteOff = fun vm -> if ensureCanWrite () then writeBit vm false
          MomentaryDown = fun vm -> if ensureCanWrite () then writeBit vm true
          MomentaryUp = fun vm -> if canWriteSilently () then writeBit vm false
          NumericWrite = fun vm value -> if ensureCanWrite () then writeWord vm value
          // 통합 스위치가 고를 수 있는 대상: 주소가 있는 요소 전부
          Targets =
            fun () ->
                state.Elements
                |> Seq.filter (fun e -> e.Enabled && e.Kind <> MasterSwitch && not (String.IsNullOrWhiteSpace e.Device))
                |> List.ofSeq
          KillAll = killAll
          IsInteractive = fun () -> not layoutMode }

    // ---------- 연결 ----------
    let applyConnectionFieldsToState () =
        if not (isNull ipBox) then state.PlcIp <- ipBox.Text.Trim()
        if not (isNull portBox) && portBox.Value.HasValue then state.Port <- int portBox.Value.Value
        if not (isNull cycleBox) && cycleBox.Value.HasValue then state.CycleMs <- int cycleBox.Value.Value

    let setConnectedUi (isConnected: bool) =
        connected <- isConnected
        // 연결과 해제를 각각 두고, 지금 할 수 있는 쪽만 켠다.
        if not (isNull connectButton) then connectButton.IsEnabled <- not isConnected
        if not (isNull disconnectButton) then disconnectButton.IsEnabled <- isConnected
        if not (isNull ipBox) then ipBox.IsEnabled <- not isConnected
        if not (isNull portBox) then portBox.IsEnabled <- not isConnected

    /// 지난 통신 오류 점등을 모두 끈다. (연결/해제할 때)
    let clearFaults () =
        runningOps.Clear()
        for e in state.Elements do
            e.Fault <- None

    let disconnect () =
        plc.Disconnect()
        clearFaults ()
        closeMonitorWindow ()
        setConnectedUi false
        writeEnabled <- false
        if not (isNull writeToggle) then
            suppressWriteToggle <- true
            writeToggle.IsChecked <- false
            suppressWriteToggle <- false
        updateWriteBadge ()
        setStatus Disconnected ""
        refreshValues ()

    let connect () =
        applyConnectionFieldsToState ()
        setStatus Connecting ""
        let ip = state.PlcIp
        let port = state.Port
        let cycle = state.CycleMs
        Task.Run(fun () ->
            let result = plc.Connect(ip, port, cycle)
            onUi (fun () ->
                match result with
                | Ok _ ->
                    clearFaults ()
                    setConnectedUi true
                    setStatus Online (DateTime.Now.ToString "HH:mm:ss.fff")
                    // 운전이 시작되면 운전 화면을 따로 띄운다.
                    openMonitorWindow ()
                    refreshValues ()
                | Error message ->
                    setConnectedUi false
                    setStatus Faulted message
                    refreshValues ()))
        |> ignore

    // ---------- 편집 명령 ----------
    let applyToScreen (announce: bool) =
        match state.Validate() with
        | Error message ->
            Dialogs.error win (I18n.tf "msg.invalid" [| box message |]) |> ignore
            false
        | Ok() ->
            rebuildCanvases ()
            refreshValues ()
            updateItemCount ()
            if announce then Dialogs.info win (I18n.t "cmd.apply") (I18n.t "msg.applied") |> ignore
            true

    let saveProject () =
        task {
            if String.IsNullOrWhiteSpace state.ProjectPath then
                let! chosen = Dialogs.saveProjectFile win "r004_hmi_project.xml"
                match chosen with
                | Some path ->
                    match saveProjectTo path with
                    | Ok() -> do! Dialogs.info win (I18n.t "cmd.save") (I18n.tf "msg.saved" [| box path |])
                    | Error m -> do! Dialogs.error win (I18n.tf "msg.saveFailed" [| box m |])
                | None -> ()
            else
                match saveProjectTo state.ProjectPath with
                | Ok() -> do! Dialogs.info win (I18n.t "cmd.save") (I18n.tf "msg.saved" [| box state.ProjectPath |])
                | Error m -> do! Dialogs.error win (I18n.tf "msg.saveFailed" [| box m |])
        }
        |> ignore

    let saveProjectAs () =
        task {
            let! chosen = Dialogs.saveProjectFile win state.ProjectPath
            match chosen with
            | Some path ->
                match saveProjectTo path with
                | Ok() -> do! Dialogs.info win (I18n.t "cmd.saveAs") (I18n.tf "msg.saved" [| box path |])
                | Error m -> do! Dialogs.error win (I18n.tf "msg.saveFailed" [| box m |])
            | None -> ()
        }
        |> ignore

    let openProject () =
        task {
            let! chosen = Dialogs.openProjectFile win
            match chosen with
            | Some path ->
                match loadProjectFrom path with
                | Ok() -> ()
                | Error m -> do! Dialogs.error win (I18n.tf "msg.loadFailed" [| box m |])
            | None -> ()
        }
        |> ignore

    let restoreSample () =
        task {
            let! yes = Dialogs.confirm win (I18n.t "cmd.sample") (I18n.t "msg.restoreSample")
            if yes then
                state.LoadProject(Project.createDefault (), state.ProjectPath)
                if not (isNull ipBox) then ipBox.Text <- state.PlcIp
                rebuildCanvases ()
                if not (isNull treeView) then treeView.Rebuild()
                state.MarkDirty()
                updateTitle ()
                updateItemCount ()
                log Info "PROJECT RESTORE r004 SAMPLE"
        }
        |> ignore

    let newProject () =
        task {
            let! yes = Dialogs.confirm win (I18n.t "cmd.new") (I18n.t "msg.restoreSample")
            if yes then
                state.LoadProject({ Project.empty with Items = [] }, "")
                rebuildCanvases ()
                if not (isNull treeView) then treeView.Rebuild()
                updateTitle ()
                updateItemCount ()
                log Info "PROJECT NEW"
        }
        |> ignore

    let copySelection () =
        let count = state.CopySelection()
        if count = 0 then Dialogs.info win (I18n.t "cmd.copy") (I18n.t "msg.selectFirst") |> ignore
        else log Info (sprintf "COPY %d ITEM(S)" count)

    let pasteSelection () =
        let count = state.PasteOffset()
        if count = 0 then Dialogs.info win (I18n.t "cmd.paste") (I18n.t "msg.copyFirst") |> ignore
        else
            applyToScreen false |> ignore
            log Info (sprintf "PASTE %d ITEM(S)" count)

    let duplicateSelection () =
        task {
            if state.SelectionCount = 0 then
                do! Dialogs.info win (I18n.t "cmd.duplicate") (I18n.t "msg.selectFirst")
            else
                let! count = Dialogs.promptCount win (I18n.t "dlg.duplicate.title") (I18n.t "dlg.duplicate.body") 1 100
                if count > 0 then
                    let added = state.DuplicateSelection count
                    applyToScreen false |> ignore
                    log Info (sprintf "DUPLICATE %d ITEM(S)" added)
        }
        |> ignore

    let addSwitchBatch () =
        task {
            let! count = Dialogs.promptCount win (I18n.t "dlg.addSwitch.title") (I18n.t "dlg.addSwitch.body") 5 100
            if count > 0 then
                let startM = state.AddSwitches count
                applyToScreen false |> ignore
                log Info (sprintf "ADD MULTI SWITCH %d / START M%d" count startM)
        }
        |> ignore

    let deleteSelection () =
        task {
            let count = state.SelectionCount
            if count > 0 then
                let message =
                    if count = 1 then
                        match state.Primary with
                        | Some vm -> I18n.tf "msg.deleteItem" [| box vm.Name |]
                        | None -> I18n.tf "msg.deleteItems" [| box count |]
                    else I18n.tf "msg.deleteItems" [| box count |]
                let! yes = Dialogs.confirm win (I18n.t "cmd.delete") message
                if yes then
                    let removed = state.RemoveSelected()
                    applyToScreen false |> ignore
                    log Info (sprintf "DELETE %d ITEM(S)" removed)
        }
        |> ignore

    let deleteOne (vm: ElementVm) =
        task {
            let! yes = Dialogs.confirm win (I18n.t "cmd.delete") (I18n.tf "msg.deleteItem" [| box vm.Name |])
            if yes then
                state.Remove [ vm ] |> ignore
                applyToScreen false |> ignore
                log Info ("DELETE " + vm.Name)
        }
        |> ignore

    /// 정렬 결과를 캔버스/상태에 반영한다.
    let afterLayoutChange (changed: int) (what: string) =
        if changed > 0 then
            applyToScreen false |> ignore
            log Info (sprintf "%s: %d ITEM(S)" what changed)

    let autoArrange () =
        // 캔버스 가로 폭에 맞춰 줄을 바꾼다.
        let width =
            if isNull canvasView then Layout.defaultCanvasWidth
            else max 400 (int canvasView.ViewportWidth)
        afterLayoutChange (state.AutoArrange width) "AUTO ARRANGE"

    let alignSelection (mode: AlignMode) (label: string) =
        if state.SelectionCount < 2 then
            Dialogs.info win (I18n.t "menu.align") (I18n.t "msg.selectTwo") |> ignore
        else
            afterLayoutChange (state.AlignSelection mode) ("ALIGN " + label)

    let distributeSelection (horizontal: bool) =
        if state.SelectionCount < 3 then
            Dialogs.info win (I18n.t "menu.align") (I18n.t "msg.selectThree") |> ignore
        else
            afterLayoutChange (state.DistributeSelection horizontal) (if horizontal then "DISTRIBUTE H" else "DISTRIBUTE V")

    let matchSelectionSize () =
        if state.SelectionCount < 2 then
            Dialogs.info win (I18n.t "menu.align") (I18n.t "msg.selectTwo") |> ignore
        else
            afterLayoutChange (state.MatchSelectionSize()) "MATCH SIZE"

    let undo () =
        if state.Undo() then
            rebuildCanvases ()
            refreshValues ()
            updateItemCount ()
            log Info "UNDO"
        else log Info "UNDO (되돌릴 내용 없음)"

    let redo () =
        if state.Redo() then
            rebuildCanvases ()
            refreshValues ()
            updateItemCount ()
            log Info "REDO"
        else log Info "REDO (다시 실행할 내용 없음)"

    /// 선택한 요소를 방향키로 미세 이동한다.
    let nudge (dx: int) (dy: int) =
        if state.SelectionCount > 0 then
            state.PushHistory()
            for vm in List.ofSeq state.Selection do
                vm.X <- max 0 (vm.X + dx)
                vm.Y <- max 0 (vm.Y + dy)
            state.GrowScreenToFit() |> ignore

    /// 운전 화면에서 추가하면 바로 보이고, 화면 편집에서 추가하면 숨긴 채로 만든다.
    /// (운전 화면은 통합 스위치만 두고 나머지는 필요할 때 켜서 쓰는 흐름)
    let addedFromRunScreen () = isNull documentTabs || documentTabs.SelectedIndex = 0

    let addElement (kind: ItemKind) =
        let vm = state.AddNew kind
        // 통합 스위치는 어디서 만들든 보인다. 그것 하나로 화면 전체를 보기 때문이다.
        if kind <> MasterSwitch && not (addedFromRunScreen ()) then vm.Visible <- false
        applyToScreen false |> ignore
        log Info (sprintf "ADD %s (%s)" kind.Code (if vm.Visible then "VISIBLE" else "HIDDEN"))

    // ---------- 설정 저장 ----------
    let persistSettings () =
        settings <-
            { settings with
                Theme = (ThemeService.current ()).Id.Code
                Language = I18n.current ()
                LastProject = state.ProjectPath
                ShowGrid = (if isNull canvasView then settings.ShowGrid else canvasView.ShowGrid)
                SnapToGrid = (if isNull canvasView then settings.SnapToGrid else canvasView.SnapToGrid)
                Zoom = (if isNull canvasView then settings.Zoom else canvasView.Zoom)
                WindowWidth = win.Width
                WindowHeight = win.Height }
        AppSettings.save settings

    let setTheme (id: ThemeId) =
        ThemeService.apply (Palette.byId id)
        persistSettings ()
        rebuildUi ()

    let setLanguage (code: string) =
        I18n.setLanguage code
        persistSettings ()
        rebuildUi ()

    // =====================================================================
    //  화면 구성
    // =====================================================================
    let buildMenu () =
        let p = ThemeService.current ()
        let menu = Menu(FontFamily = Ui.uiFont)

        let fileMenu = MenuItem(Header = I18n.t "menu.file")
        fileMenu.Items.Add(Ui.menuItem (I18n.t "cmd.new") newProject) |> ignore
        fileMenu.Items.Add(Ui.menuItem (I18n.t "cmd.open") openProject) |> ignore
        fileMenu.Items.Add(Separator()) |> ignore
        fileMenu.Items.Add(Ui.menuItem (I18n.t "cmd.save") saveProject) |> ignore
        fileMenu.Items.Add(Ui.menuItem (I18n.t "cmd.saveAs") saveProjectAs) |> ignore
        fileMenu.Items.Add(Separator()) |> ignore
        fileMenu.Items.Add(Ui.menuItem (I18n.t "cmd.sample") restoreSample) |> ignore
        fileMenu.Items.Add(Separator()) |> ignore
        fileMenu.Items.Add(Ui.menuItem (I18n.t "cmd.exit") (fun () -> win.Close())) |> ignore
        menu.Items.Add fileMenu |> ignore

        let editMenu = MenuItem(Header = I18n.t "menu.edit")
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.undo") undo) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.redo") redo) |> ignore
        editMenu.Items.Add(Separator()) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.copy") copySelection) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.paste") pasteSelection) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.duplicate") duplicateSelection) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.delete") deleteSelection) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.selectAll") (fun () -> state.SelectAll())) |> ignore
        editMenu.Items.Add(Separator()) |> ignore
        for kind in ItemKind.all do
            editMenu.Items.Add(Ui.menuItem ("+ " + I18n.kindLabel kind) (fun () -> addElement kind)) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.addSwitchBatch") addSwitchBatch) |> ignore
        editMenu.Items.Add(Separator()) |> ignore

        let alignMenu = MenuItem(Header = I18n.t "menu.align")
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.auto") autoArrange) |> ignore
        alignMenu.Items.Add(Separator()) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.left") (fun () -> alignSelection Left "LEFT")) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.centerH") (fun () -> alignSelection CenterHorizontal "CENTER-H")) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.right") (fun () -> alignSelection Right "RIGHT")) |> ignore
        alignMenu.Items.Add(Separator()) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.top") (fun () -> alignSelection Top "TOP")) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.middle") (fun () -> alignSelection Middle "MIDDLE")) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.bottom") (fun () -> alignSelection Bottom "BOTTOM")) |> ignore
        alignMenu.Items.Add(Separator()) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.distH") (fun () -> distributeSelection true)) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.distV") (fun () -> distributeSelection false)) |> ignore
        alignMenu.Items.Add(Ui.menuItem (I18n.t "align.sameSize") matchSelectionSize) |> ignore
        editMenu.Items.Add alignMenu |> ignore

        editMenu.Items.Add(Separator()) |> ignore
        editMenu.Items.Add(Ui.menuItem (I18n.t "cmd.apply") (fun () -> applyToScreen true |> ignore)) |> ignore
        menu.Items.Add editMenu |> ignore

        let viewMenu = MenuItem(Header = I18n.t "menu.view")
        let themeMenu = MenuItem(Header = I18n.t "menu.theme")
        for pal in Palette.all do
            themeMenu.Items.Add(Ui.radioMenuItem (I18n.t pal.Id.NameKey) (pal.Id = p.Id) (fun () -> setTheme pal.Id))
            |> ignore
        viewMenu.Items.Add themeMenu |> ignore

        let langMenu = MenuItem(Header = I18n.t "menu.language")
        for lang in I18n.languages do
            let header = if lang.Native = lang.English then lang.Native else lang.Native + "  ·  " + lang.English
            langMenu.Items.Add(Ui.radioMenuItem header (lang.Code = I18n.current ()) (fun () -> setLanguage lang.Code))
            |> ignore
        viewMenu.Items.Add langMenu |> ignore
        viewMenu.Items.Add(Separator()) |> ignore

        let panelsMenu = MenuItem(Header = I18n.t "menu.panels")
        panelsMenu.Items.Add(
            Ui.checkableMenuItem (I18n.t "panel.project") showProjectPanel (fun () ->
                showProjectPanel <- not showProjectPanel
                rebuildUi ()))
        |> ignore
        panelsMenu.Items.Add(
            Ui.checkableMenuItem (I18n.t "panel.property") showPropertyPanel (fun () ->
                showPropertyPanel <- not showPropertyPanel
                rebuildUi ()))
        |> ignore
        panelsMenu.Items.Add(
            Ui.checkableMenuItem (I18n.t "panel.output") showOutputPanel (fun () ->
                showOutputPanel <- not showOutputPanel
                rebuildUi ()))
        |> ignore
        viewMenu.Items.Add panelsMenu |> ignore
        viewMenu.Items.Add(Separator()) |> ignore
        viewMenu.Items.Add(
            Ui.checkableMenuItem (I18n.t "cmd.showGrid") (not (isNull canvasView) && canvasView.ShowGrid) (fun () ->
                if not (isNull canvasView) then
                    canvasView.ShowGrid <- not canvasView.ShowGrid
                    persistSettings ()))
        |> ignore
        viewMenu.Items.Add(
            Ui.checkableMenuItem (I18n.t "cmd.snap") (not (isNull canvasView) && canvasView.SnapToGrid) (fun () ->
                if not (isNull canvasView) then
                    canvasView.SnapToGrid <- not canvasView.SnapToGrid
                    persistSettings ()))
        |> ignore
        viewMenu.Items.Add(Separator()) |> ignore
        viewMenu.Items.Add(
            Ui.menuItem (I18n.t "cmd.fitToWindow") (fun () ->
                if not (isNull canvasView) then canvasView.FitToWindow()))
        |> ignore
        viewMenu.Items.Add(
            Ui.checkableMenuItem (I18n.t "cmd.monitorWindow") (not (isNull monitorWindow)) (fun () ->
                if isNull monitorWindow then openMonitorWindow () else closeMonitorWindow ()))
        |> ignore
        menu.Items.Add viewMenu |> ignore

        let onlineMenu = MenuItem(Header = I18n.t "menu.online")
        onlineMenu.Items.Add(Ui.menuItem (I18n.t "cmd.connect") (fun () -> if not connected then connect ())) |> ignore
        onlineMenu.Items.Add(Ui.menuItem (I18n.t "cmd.disconnect") (fun () -> if connected then disconnect ())) |> ignore
        menu.Items.Add onlineMenu |> ignore

        let toolsMenu = MenuItem(Header = I18n.t "menu.tools")
        toolsMenu.Items.Add(
            Ui.menuItem (I18n.t "cmd.clearLog") (fun () ->
                logHistory.Clear()
                if not (isNull output) then output.Clear()))
        |> ignore
        menu.Items.Add toolsMenu |> ignore

        let helpMenu = MenuItem(Header = I18n.t "menu.help")
        helpMenu.Items.Add(Ui.menuItem (I18n.t "about.title") (fun () -> Dialogs.about win |> ignore)) |> ignore
        menu.Items.Add helpMenu |> ignore

        menu

    let buildToolbar () =
        let p = ThemeService.current ()

        ipBox <- TextBox(Text = state.PlcIp, Width = 148.0, FontFamily = Ui.monoFont)
        portBox <- NumericUpDown(Minimum = 1m, Maximum = 65535m, Value = decimal state.Port, Increment = 1m, FormatString = "0", Width = 124.0)
        cycleBox <-
            NumericUpDown(
                Minimum = decimal Limits.minCycleMs,
                Maximum = decimal Limits.maxCycleMs,
                Value = decimal state.CycleMs,
                Increment = 50m,
                FormatString = "0",
                Width = 126.0
            )
        cycleBox.ValueChanged.Add(fun _ ->
            if cycleBox.Value.HasValue then
                state.CycleMs <- int cycleBox.Value.Value
                plc.CycleMs <- int cycleBox.Value.Value)

        connectButton <- Ui.button (I18n.t "cmd.connect") [ "primary" ] (fun () -> if not connected then connect ())
        connectButton.MinWidth <- 88.0
        disconnectButton <- Ui.button (I18n.t "cmd.disconnect") [ "danger" ] (fun () -> if connected then disconnect ())
        disconnectButton.MinWidth <- 88.0

        writeToggle <-
            Ui.toggleButton (I18n.t "cmd.writeEnable") [ "warn" ] false (fun isChecked ->
                if not suppressWriteToggle then
                    if isChecked then
                        task {
                            let! yes = Dialogs.confirm win (I18n.t "msg.writeEnable.title") (I18n.t "msg.writeEnable.body")
                            if yes then
                                writeEnabled <- true
                                log Warn "PLC WRITE ENABLED"
                            else
                                suppressWriteToggle <- true
                                writeToggle.IsChecked <- false
                                suppressWriteToggle <- false
                                writeEnabled <- false
                            updateWriteBadge ()
                        }
                        |> ignore
                    else
                        writeEnabled <- false
                        log Info "PLC WRITE LOCKED"
                        updateWriteBadge ())

        let labelled (caption: string) (editor: Control) =
            let l = Ui.muted caption
            l.FontSize <- 11.0
            Ui.stackH 6.0 [ l; editor ] :> Control

        let group (children: Control seq) =
            let s = Ui.stackH 5.0 children
            s.Margin <- Thickness(0.0, 2.0, 0.0, 2.0)
            s :> Control

        // 화면 편집에 쓰는 것들은 툴바에 늘어놓지 않고 메뉴 항목으로 만든다.
        let layoutItem =
            MenuItem(
                Header = I18n.t "cmd.layoutMode",
                ToggleType = MenuItemToggleType.CheckBox,
                IsChecked = layoutMode,
                FontFamily = Ui.uiFont
            )
        // 체크 상태는 setLayoutMode 가 정해 주므로 여기서는 내 상태만 뒤집는다.
        layoutItem.Click.Add(fun _ -> setLayoutMode (not layoutMode))
        layoutModeItem <- layoutItem

        let gridItem =
            MenuItem(
                Header = I18n.t "cmd.showGrid",
                ToggleType = MenuItemToggleType.CheckBox,
                IsChecked = settings.ShowGrid,
                FontFamily = Ui.uiFont
            )
        gridItem.Click.Add(fun _ ->
            if not (isNull canvasView) then
                let on = not canvasView.ShowGrid
                canvasView.ShowGrid <- on
                gridItem.IsChecked <- on
                persistSettings ())

        let snapItem =
            MenuItem(
                Header = I18n.t "cmd.snap",
                ToggleType = MenuItemToggleType.CheckBox,
                IsChecked = settings.SnapToGrid,
                FontFamily = Ui.uiFont
            )
        snapItem.Click.Add(fun _ ->
            if not (isNull canvasView) then
                let on = not canvasView.SnapToGrid
                canvasView.SnapToGrid <- on
                snapItem.IsChecked <- on
                persistSettings ())

        let screenWidthBox =
            NumericUpDown(
                Minimum = decimal Limits.minScreenWidth,
                Maximum = decimal Limits.maxScreenSize,
                Value = decimal state.ScreenWidth,
                Increment = 100m,
                FormatString = "0",
                Width = 116.0
            )
        let screenHeightBox =
            NumericUpDown(
                Minimum = decimal Limits.minScreenHeight,
                Maximum = decimal Limits.maxScreenSize,
                Value = decimal state.ScreenHeight,
                Increment = 100m,
                FormatString = "0",
                Width = 116.0
            )
        screenWidthBox.ValueChanged.Add(fun _ ->
            if screenWidthBox.Value.HasValue then
                state.ScreenWidth <- int screenWidthBox.Value.Value)
        screenHeightBox.ValueChanged.Add(fun _ ->
            if screenHeightBox.Value.HasValue then
                state.ScreenHeight <- int screenHeightBox.Value.Value)

        // 요소를 밖으로 끌어 도면이 넓어지면 입력칸도 따라간다.
        state.ScreenChanged.Add(fun () ->
            if screenWidthBox.Value <> System.Nullable(decimal state.ScreenWidth) then
                screenWidthBox.Value <- decimal state.ScreenWidth
            if screenHeightBox.Value <> System.Nullable(decimal state.ScreenHeight) then
                screenHeightBox.Value <- decimal state.ScreenHeight)

        zoomLabel <- Ui.mono 11.5 (sprintf "%d%%" (int (settings.Zoom * 100.0)))
        zoomLabel.Foreground <- Ui.brush p.TextMuted

        let setZoom (value: float) =
            if not (isNull canvasView) then
                canvasView.Zoom <- value
                zoomLabel.Text <- sprintf "%d%%" (int (canvasView.Zoom * 100.0))
                persistSettings ()

        let wrap = WrapPanel(Orientation = Orientation.Horizontal, Margin = Thickness(8.0, 5.0, 8.0, 5.0), ItemSpacing = 4.0, LineSpacing = 4.0)

        // 도면 크기는 입력칸이라 메뉴 안에서도 눌러 고칠 수 있게 그대로 넣는다.
        let screenSizeRow =
            let row = Ui.stackH 5.0 [ Ui.text (I18n.t "screen.size"); screenWidthBox; Ui.text "×"; screenHeightBox ]
            row.VerticalAlignment <- VerticalAlignment.Center
            row :> Control

        // 화면 편집에 쓰는 버튼을 전부 이 버튼 하나에 담는다. 운전 화면 툴바는 접속과 보기만 남는다.
        let editButton =
            let sub (header: string) (children: Control list) = Ui.subMenu header children :> Control
            let item (header: string) (action: unit -> unit) = Ui.menuItem header action :> Control
            Ui.menuButton
                (I18n.t "cmd.editTools")
                ""
                [ sub (I18n.t "cmd.addElement") (ItemKind.all |> List.map (fun kind -> item (I18n.kindLabel kind) (fun () -> addElement kind)))
                  item (I18n.t "cmd.addSwitchBatch") addSwitchBatch
                  Ui.separatorItem ()
                  layoutItem :> Control
                  gridItem :> Control
                  snapItem :> Control
                  Ui.separatorItem ()
                  item (I18n.t "cmd.undo") undo
                  item (I18n.t "cmd.redo") redo
                  Ui.separatorItem ()
                  item (I18n.t "cmd.copy") copySelection
                  item (I18n.t "cmd.paste") pasteSelection
                  item (I18n.t "cmd.duplicate") duplicateSelection
                  item (I18n.t "cmd.delete") deleteSelection
                  Ui.separatorItem ()
                  sub
                      (I18n.t "menu.align")
                      [ item (I18n.t "align.auto") autoArrange
                        item (I18n.t "align.left") (fun () -> alignSelection Left "LEFT")
                        item (I18n.t "align.top") (fun () -> alignSelection Top "TOP")
                        item (I18n.t "align.distH") (fun () -> distributeSelection true)
                        item (I18n.t "align.distV") (fun () -> distributeSelection false)
                        item (I18n.t "align.sameSize") matchSelectionSize ]
                  Ui.controlMenuItem screenSizeRow :> Control
                  Ui.separatorItem ()
                  item (I18n.t "cmd.apply") (fun () -> applyToScreen true |> ignore) ]

        let items: Control list =
            [ group [ labelled (I18n.t "conn.ip") ipBox
                      labelled (I18n.t "conn.port") portBox
                      labelled (I18n.t "conn.cycle") cycleBox
                      connectButton
                      disconnectButton
                      writeToggle ]
              Ui.vSep ()
              group [ editButton :> Control ]
              Ui.vSep ()
              group [ Ui.toolButton (I18n.t "cmd.fitToWindow") "" (fun () ->
                          if not (isNull canvasView) then
                              canvasView.FitToWindow()
                              zoomLabel.Text <- sprintf "%d%%" (int (canvasView.Zoom * 100.0))
                              persistSettings ())
                      Ui.toolButton "−" (I18n.t "cmd.zoomOut") (fun () -> setZoom ((if isNull canvasView then 1.0 else canvasView.Zoom) - 0.1))
                      zoomLabel
                      Ui.toolButton "＋" (I18n.t "cmd.zoomIn") (fun () -> setZoom ((if isNull canvasView then 1.0 else canvasView.Zoom) + 0.1))
                      Ui.toolButton "1:1" (I18n.t "cmd.zoomReset") (fun () -> setZoom 1.0) ] ]

        for i in items do
            wrap.Children.Add i

        Border(Background = Ui.brush p.Header, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(0.0, 0.0, 0.0, 1.0), Child = wrap)

    let buildStatusBar () =
        let p = ThemeService.current ()

        let pillText = Ui.text (I18n.t "status.disconnected")
        pillText.FontSize <- 11.5
        pillText.FontWeight <- FontWeight.SemiBold
        statusPill <- Border(Background = Ui.tint p.Off 0.25, CornerRadius = CornerRadius 9.0, Padding = Thickness(10.0, 2.0, 10.0, 3.0), Child = pillText)

        statusText <- Ui.text ""
        statusText.FontSize <- 11.5
        statusText.Foreground <- Ui.brush p.TextMuted

        statusProfile <- Ui.text ""
        statusProfile.FontSize <- 11.5
        statusProfile.Foreground <- Ui.brush p.TextMuted

        statusItems <- Ui.text ""
        statusItems.FontSize <- 11.5
        statusItems.Foreground <- Ui.brush p.TextMuted

        statusGeometry <- Ui.mono 11.0 ""
        statusGeometry.Foreground <- Ui.brush p.Accent

        let writeText = Ui.text (I18n.t "status.writeLocked")
        writeText.FontSize <- 11.0
        writeText.FontWeight <- FontWeight.SemiBold
        statusWrite <- Border(Background = Ui.tint p.Off 0.22, CornerRadius = CornerRadius 4.0, Padding = Thickness(8.0, 2.0, 8.0, 3.0), Child = writeText)

        let themeName = Ui.text (I18n.t (ThemeService.current ()).Id.NameKey)
        themeName.FontSize <- 11.0
        themeName.Foreground <- Ui.brush p.TextMuted

        let langName = Ui.text (I18n.currentLanguage ()).Native
        langName.FontSize <- 11.0
        langName.Foreground <- Ui.brush p.TextMuted

        let left = Ui.stackH 10.0 [ statusPill; statusText; statusProfile ]
        let right = Ui.stackH 10.0 [ statusGeometry; statusItems; statusWrite; themeName; langName ]
        right.HorizontalAlignment <- HorizontalAlignment.Right

        let bar = Grid(Margin = Thickness(10.0, 3.0, 10.0, 3.0))
        bar.ColumnDefinitions.Add(ColumnDefinition(GridLength.Auto))
        bar.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
        Grid.SetColumn(left, 0)
        Grid.SetColumn(right, 1)
        bar.Children.Add left
        bar.Children.Add right

        Border(Background = Ui.brush p.StatusBar, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(0.0, 1.0, 0.0, 0.0), Child = bar, Height = 28.0)

    let buildDocumentArea () =
        let p = ThemeService.current ()

        let host: CanvasHost =
            { Cards = cardCallbacks
              CopyItem =
                fun vm ->
                    state.CopyOne vm
                    log Info ("COPY RUNTIME " + vm.Name)
              DuplicateItem =
                fun vm ->
                    state.DuplicateOne vm |> ignore
                    applyToScreen false |> ignore
                    log Info ("DUPLICATE RUNTIME " + vm.Name)
              DeleteItem = fun vm -> deleteOne vm
              PasteAt =
                fun x y ->
                    let count = state.PasteAt(x, y)
                    if count = 0 then Dialogs.info win (I18n.t "cmd.paste") (I18n.t "msg.copyFirst") |> ignore
                    else
                        applyToScreen false |> ignore
                        log Info (sprintf "PASTE TO CANVAS %d ITEM(S)" count)
              Info = fun text -> if not (isNull statusGeometry) then statusGeometry.Text <- text }

        canvasHost <- Some host
        canvasView <- new RunCanvasView(state, host)
        canvasView.ShowGrid <- settings.ShowGrid
        canvasView.SnapToGrid <- settings.SnapToGrid
        canvasView.Zoom <- settings.Zoom
        canvasView.LayoutMode <- layoutMode
        canvasView.ZoomChanged.Add(fun z ->
            if not (isNull zoomLabel) then zoomLabel.Text <- sprintf "%d%%" (int (z * 100.0)))
        canvasView.Rebuild()

        tableView <- new ElementTableView(state)
        let tableMenu = ContextMenu()
        tableMenu.Items.Add(Ui.menuItem (I18n.t "cmd.copy") copySelection) |> ignore
        tableMenu.Items.Add(Ui.menuItem (I18n.t "cmd.paste") pasteSelection) |> ignore
        tableMenu.Items.Add(Ui.menuItem (I18n.t "cmd.duplicate") duplicateSelection) |> ignore
        tableMenu.Items.Add(Separator()) |> ignore
        tableMenu.Items.Add(Ui.menuItem (I18n.t "cmd.delete") deleteSelection) |> ignore
        tableView.SetContextMenu tableMenu

        let hint = Ui.muted (I18n.t "canvas.hint")
        hint.FontSize <- 11.0
        hint.Margin <- Thickness(10.0, 4.0, 10.0, 4.0)
        hint.TextWrapping <- TextWrapping.Wrap
        layoutHintText <- hint

        let runTab = TabItem(Header = I18n.t "tab.run")
        let runRoot = DockPanel(LastChildFill = true)
        let hintBar = Border(Background = Ui.tint p.Accent 0.08, Child = hint)
        layoutHintBar <- hintBar
        setLayoutHint layoutMode
        DockPanel.SetDock(hintBar, Dock.Top)
        runRoot.Children.Add hintBar
        runRoot.Children.Add canvasView.Root
        runTab.Content <- runRoot

        let tableTab = TabItem(Header = I18n.t "tab.table")
        tableTab.Content <- tableView.Root

        let tabs = TabControl(Background = Ui.brush p.Surface)
        tabs.Items.Add runTab |> ignore
        tabs.Items.Add tableTab |> ignore
        documentTabs <- tabs
        tabs

    /// 테마/언어를 바꿀 때 이전 화면이 붙잡고 있던 구독을 모두 끊는다. (누수 방지)
    let disposeViews () =
        let dispose (o: obj) =
            match o with
            | :? IDisposable as d -> d.Dispose()
            | _ -> ()
        dispose canvasView
        dispose tableView
        dispose treeView
        dispose propertyView

    let buildShell () =
        disposeViews ()
        let p = ThemeService.current ()
        win.Background <- Ui.brush p.Window
        win.FlowDirection <- (if I18n.isRtl () then FlowDirection.RightToLeft else FlowDirection.LeftToRight)

        // 왼쪽: 프로젝트 트리
        treeView <- new ProjectTreeView(state)
        let projectPanel =
            let d = DockPanel(LastChildFill = true)
            let header = Ui.panelHeader (I18n.t "panel.project") None
            DockPanel.SetDock(header, Dock.Top)
            d.Children.Add header
            d.Children.Add(ScrollViewer(Content = treeView.Root, VerticalScrollBarVisibility = Primitives.ScrollBarVisibility.Auto))
            Border(Background = Ui.brush p.Surface, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(0.0, 0.0, 1.0, 0.0), Child = d)

        // 오른쪽: 속성
        propertyView <- new PropertyPanelView(state)
        let propertyPanel =
            let d = DockPanel(LastChildFill = true)
            let header = Ui.panelHeader (I18n.t "panel.property") None
            DockPanel.SetDock(header, Dock.Top)
            d.Children.Add header
            d.Children.Add propertyView.Root
            Border(Background = Ui.brush p.Surface, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(1.0, 0.0, 0.0, 0.0), Child = d)

        // 아래: 출력
        output <- OutputPanelView()
        for record in logHistory do
            output.Append(record.Level, record.Message)
        let outputPanel =
            let d = DockPanel(LastChildFill = true)

            let traceToggle =
                Ui.toggleButton (I18n.t "log.trace") [] plc.TraceEnabled (fun isChecked ->
                    plc.TraceEnabled <- isChecked
                    log Info ("TRACE " + (if isChecked then "ON" else "OFF")))
            let changesToggle =
                Ui.toggleButton (I18n.t "log.changes") [] plc.LogChanges (fun isChecked ->
                    plc.LogChanges <- isChecked
                    log Info ("CHANGE LOG " + (if isChecked then "ON" else "OFF")))

            let stats = Ui.mono 10.5 ""
            stats.Foreground <- Ui.brush p.TextMuted
            logStatsText <- stats

            let clearButton = Ui.toolButton (I18n.t "cmd.clearLog") "" (fun () ->
                logHistory.Clear()
                output.Clear())

            let tools = Ui.stackH 4.0 [ stats; traceToggle; changesToggle; clearButton ]
            tools.Margin <- Thickness(0.0, 0.0, 6.0, 0.0)
            let header = Ui.panelHeader (I18n.t "panel.output") (Some(tools :> Control))
            DockPanel.SetDock(header, Dock.Top)
            d.Children.Add header
            d.Children.Add output.Root
            Border(Background = Ui.brush p.Surface, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(0.0, 1.0, 0.0, 0.0), Child = d)

        let documents = buildDocumentArea ()

        // 가운데 세로 분할: 문서 / 출력
        let center = Grid()
        center.RowDefinitions.Add(RowDefinition(GridLength(1.0, GridUnitType.Star)))
        if showOutputPanel then
            center.RowDefinitions.Add(RowDefinition(GridLength(4.0, GridUnitType.Pixel)))
            center.RowDefinitions.Add(RowDefinition(GridLength(190.0, GridUnitType.Pixel), MinHeight = 90.0))
        Grid.SetRow(documents, 0)
        center.Children.Add documents
        if showOutputPanel then
            let splitter = GridSplitter(Height = 4.0, ResizeDirection = GridResizeDirection.Rows, HorizontalAlignment = HorizontalAlignment.Stretch)
            Grid.SetRow(splitter, 1)
            Grid.SetRow(outputPanel, 2)
            center.Children.Add splitter
            center.Children.Add outputPanel

        // 좌우 도킹
        let body = Grid()
        let mutable column = 0
        if showProjectPanel then
            body.ColumnDefinitions.Add(ColumnDefinition(GridLength(268.0, GridUnitType.Pixel), MinWidth = 150.0))
            body.ColumnDefinitions.Add(ColumnDefinition(GridLength(4.0, GridUnitType.Pixel)))
            Grid.SetColumn(projectPanel, 0)
            body.Children.Add projectPanel
            let splitter = GridSplitter(Width = 4.0, ResizeDirection = GridResizeDirection.Columns)
            Grid.SetColumn(splitter, 1)
            body.Children.Add splitter
            column <- 2
        body.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
        Grid.SetColumn(center, column)
        body.Children.Add center
        if showPropertyPanel then
            body.ColumnDefinitions.Add(ColumnDefinition(GridLength(4.0, GridUnitType.Pixel)))
            body.ColumnDefinitions.Add(ColumnDefinition(GridLength(300.0, GridUnitType.Pixel), MinWidth = 180.0))
            let splitter = GridSplitter(Width = 4.0, ResizeDirection = GridResizeDirection.Columns)
            Grid.SetColumn(splitter, column + 1)
            Grid.SetColumn(propertyPanel, column + 2)
            body.Children.Add splitter
            body.Children.Add propertyPanel

        // 제목 줄 (프로젝트 이름)
        titleText <- Ui.text (projectLabel ())
        titleText.FontSize <- 12.0
        titleText.Foreground <- Ui.brush p.TextMuted

        let titleBar =
            let d = DockPanel(LastChildFill = true, Margin = Thickness(10.0, 0.0, 12.0, 0.0), Height = 30.0)
            DockPanel.SetDock(titleText, Dock.Right)
            d.Children.Add titleText
            d.Children.Add(buildMenu ())
            Border(Background = Ui.brush p.Header, BorderBrush = Ui.brush p.Border, BorderThickness = Thickness(0.0, 0.0, 0.0, 1.0), Child = d)

        let root = DockPanel(LastChildFill = true)
        DockPanel.SetDock(titleBar, Dock.Top)
        root.Children.Add titleBar
        let toolbar = buildToolbar ()
        DockPanel.SetDock(toolbar, Dock.Top)
        root.Children.Add toolbar
        let statusBar = buildStatusBar ()
        DockPanel.SetDock(statusBar, Dock.Bottom)
        root.Children.Add statusBar
        root.Children.Add body

        win.Content <- root

        setConnectedUi connected
        suppressWriteToggle <- true
        writeToggle.IsChecked <- writeEnabled
        suppressWriteToggle <- false
        updateWriteBadge ()
        setStatus (if connected then Online else Disconnected) ""
        updateItemCount ()
        updateTitle ()
        updateLogStats ()
        setLayoutHint layoutMode
        refreshValues ()

    rebuildUi <- fun () -> buildShell ()

    // ---------- 이벤트 연결 ----------
    plc.Log.Add(fun (level, message) -> onUi (fun () -> log level message))

    plc.StateChanged.Add(fun (kind, detail) ->
        onUi (fun () ->
            match kind with
            | Online ->
                setStatus Online detail
                updateLogStats ()
            | Faulted ->
                setStatus Faulted detail
                updateLogStats ()
                // 통신이 끊기면 운전 화면 카드를 모두 빨간색으로 점등한다.
                refreshValues ()
            | Connecting -> setStatus Connecting ""
            | Disconnected -> setStatus Disconnected ""))

    plc.ValuesChanged.Add(fun () -> onUi refreshValues)

    plc.SetScanProvider(fun () -> state.ScanAddresses())

    state.StructureChanged.Add(fun () ->
        updateItemCount ()
        updateTitle ())

    state.SelectionChanged.Add(fun () -> updateItemCount ())

    state.DirtyChanged.Add(fun _ -> updateTitle ())

    state.ItemChanged.Add(fun (vm, prop) ->
        if not (isNull canvasView) then
            match prop with
            // 카드 구조가 바뀌는 항목: 그 카드만 다시 만든다.
            | "Kind" | "Action" | "Enabled" | "Visible" | "Name" | "Device" | "MonitorDevice" | "Min" | "Max" ->
                for c in canvases () do
                    c.RebuildOne vm
                refreshValues ()
            | _ -> ())

    // ---------- 단축키 ----------
    win.KeyDown.Add(fun e ->
        let ctrl = e.KeyModifiers.HasFlag KeyModifiers.Control || e.KeyModifiers.HasFlag KeyModifiers.Meta
        match e.Key with
        | Key.C when ctrl ->
            copySelection ()
            e.Handled <- true
        | Key.V when ctrl ->
            pasteSelection ()
            e.Handled <- true
        | Key.S when ctrl ->
            saveProject ()
            e.Handled <- true
        | Key.O when ctrl ->
            openProject ()
            e.Handled <- true
        | Key.A when ctrl ->
            state.SelectAll()
            e.Handled <- true
        | Key.Z when ctrl && e.KeyModifiers.HasFlag KeyModifiers.Shift ->
            redo ()
            e.Handled <- true
        | Key.Z when ctrl ->
            undo ()
            e.Handled <- true
        | Key.Y when ctrl ->
            redo ()
            e.Handled <- true
        | Key.D when ctrl ->
            (match state.Primary with
             | Some vm ->
                 state.DuplicateOne vm |> ignore
                 applyToScreen false |> ignore
                 log Info ("DUPLICATE " + vm.Name)
             | None -> Dialogs.info win (I18n.t "cmd.duplicate") (I18n.t "msg.selectFirst") |> ignore)
            e.Handled <- true
        | Key.Left | Key.Right | Key.Up | Key.Down when layoutMode ->
            // 입력칸에 글자를 쓰는 중이면 방향키를 가로채지 않는다.
            let editing =
                match win.FocusManager with
                | null -> false
                | fm ->
                    let rec isEditor (v: Visual) =
                        if isNull v then false
                        else
                            match v with
                            | :? TextBox -> true
                            | :? NumericUpDown -> true
                            | _ -> isEditor (v.GetVisualParent())
                    match fm.GetFocusedElement() with
                    | :? Visual as focused -> isEditor focused
                    | _ -> false
            if not editing then
                let step = if e.KeyModifiers.HasFlag KeyModifiers.Shift then 10 else 1
                (match e.Key with
                 | Key.Left -> nudge -step 0
                 | Key.Right -> nudge step 0
                 | Key.Up -> nudge 0 -step
                 | Key.Down -> nudge 0 step
                 | _ -> ())
                e.Handled <- true
        | Key.Delete ->
            deleteSelection ()
            e.Handled <- true
        | Key.F5 ->
            (if connected then disconnect () else connect ())
            e.Handled <- true
        | Key.F2 ->
            setLayoutMode (not layoutMode)
            e.Handled <- true
        | _ -> ())

    win.Closing.Add(fun _ ->
        persistSettings ()
        closeMonitorWindow ()
        plc.Disconnect())

    // ---------- 첫 로드 ----------
    let startupPath =
        if not (String.IsNullOrWhiteSpace settings.LastProject) && File.Exists settings.LastProject then settings.LastProject
        else ProjectIo.defaultProjectPath ()

    let project = ProjectIo.loadOrDefault startupPath
    state.LoadProject(project, startupPath)
    plc.CycleMs <- project.CycleMs

    buildShell ()
    log Info (sprintf "%s — %s" (I18n.t "app.title") (I18n.t "app.edition"))
    log Info ("PROJECT " + startupPath)
    log Warn (I18n.t "safety.banner")

    win, (fun () -> rebuildUi ())

let create (initialSettings: AppSettings) : Window =
    fst (createWithRebuild initialSettings)
