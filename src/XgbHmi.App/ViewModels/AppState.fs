namespace XgbHmi.App.ViewModels

open System
open System.Collections.ObjectModel
open System.Collections.Specialized
open XgbHmi.Core

/// 프로젝트 편집 상태 전체. 표 / 트리 / 캔버스 / 속성창이 이 하나를 공유한다.
type AppState() =

    let elements = ObservableCollection<ElementVm>()
    let selection = ResizeArray<ElementVm>()

    let structureChanged = Event<unit>()
    let selectionChanged = Event<unit>()
    let itemChanged = Event<ElementVm * string>()
    let dirtyChanged = Event<bool>()

    let mutable projectPath = ""
    let mutable plcIp = Limits.defaultIp
    let mutable port = Limits.defaultPort
    let mutable cycleMs = Limits.defaultCycleMs
    let mutable copyBuffer: HmiItem list = []
    let mutable pasteSerial = 0
    let mutable dirty = false
    let mutable screenWidth = Limits.defaultScreenWidth
    let mutable screenHeight = Limits.defaultScreenHeight

    let screenChanged = Event<unit>()
    let historyChanged = Event<unit>()

    // 되돌리기 / 다시 실행 (요소 목록 전체를 스냅숏으로 보관한다)
    let undoStack = System.Collections.Generic.Stack<HmiItem list>()
    let redoStack = System.Collections.Generic.Stack<HmiItem list>()
    let maxHistory = 60
    let mutable lastEditTicks = 0L
    /// 되돌리기 적용 중에는 스냅숏을 남기지 않는다.
    let mutable restoring = false

    let setDirty v =
        if dirty <> v then
            dirty <- v
            dirtyChanged.Trigger v

    let currentItems () =
        elements |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq

    let trimHistory () =
        if undoStack.Count > maxHistory then
            let keep = undoStack.ToArray() |> Array.truncate maxHistory |> Array.rev
            undoStack.Clear()
            for item in keep do
                undoStack.Push item

    /// 바꾸기 직전 상태를 되돌리기 더미에 쌓는다. 짧은 시간에 몰아치는 변경은 하나로 묶는다.
    let pushHistory (coalesceMs: int64) =
        if not restoring then
            let now = DateTime.UtcNow.Ticks / 10000L
            if coalesceMs <= 0L || now - lastEditTicks > coalesceMs then
                undoStack.Push(currentItems ())
                redoStack.Clear()
                trimHistory ()
                historyChanged.Trigger()
            lastEditTicks <- now

    let attach (vm: ElementVm) =
        vm.SetBeforeChangeHook(fun () -> pushHistory 700L)
        vm.PropertyChangedEvent.Add(fun args ->
            setDirty true
            itemChanged.Trigger(vm, args.PropertyName))
        vm

    member _.Elements = elements
    member _.StructureChanged = structureChanged.Publish
    member _.ScreenChanged = screenChanged.Publish
    member _.HistoryChanged = historyChanged.Publish

    /// 배치할 수 있는 도면 크기 (스크롤 영역)
    member _.ScreenWidth
        with get () = screenWidth
        and set v =
            let v = max Limits.minScreenWidth (min Limits.maxScreenSize v)
            if screenWidth <> v then
                screenWidth <- v
                setDirty true
                screenChanged.Trigger()

    member _.ScreenHeight
        with get () = screenHeight
        and set v =
            let v = max Limits.minScreenHeight (min Limits.maxScreenSize v)
            if screenHeight <> v then
                screenHeight <- v
                setDirty true
                screenChanged.Trigger()

    /// 요소가 도면 밖으로 나가면 도면을 자동으로 넓힌다.
    member this.GrowScreenToFit() =
        let needWidth = elements |> Seq.fold (fun acc (e: ElementVm) -> max acc (e.X + e.Width + 40)) Limits.minScreenWidth
        let needHeight = elements |> Seq.fold (fun acc (e: ElementVm) -> max acc (e.Y + e.Height + 40)) Limits.minScreenHeight
        let mutable grew = false
        if needWidth > screenWidth then
            screenWidth <- min Limits.maxScreenSize needWidth
            grew <- true
        if needHeight > screenHeight then
            screenHeight <- min Limits.maxScreenSize needHeight
            grew <- true
        if grew then
            setDirty true
            screenChanged.Trigger()
        grew

    member _.CanUndo = undoStack.Count > 0
    member _.CanRedo = redoStack.Count > 0

    /// 바꾸기 직전 상태를 기록한다. (명령 실행 전에 부른다)
    member _.PushHistory() = pushHistory 0L

    member private _.RestoreItems(items: HmiItem list) =
        restoring <- true
        selection.Clear()
        elements.Clear()
        for item in items do
            elements.Add(attach (ElementVm item))
        restoring <- false
        setDirty true
        structureChanged.Trigger()
        selectionChanged.Trigger()
        historyChanged.Trigger()

    member this.Undo() =
        if undoStack.Count = 0 then false
        else
            redoStack.Push(currentItems ())
            this.RestoreItems(undoStack.Pop())
            lastEditTicks <- 0L
            true

    member this.Redo() =
        if redoStack.Count = 0 then false
        else
            undoStack.Push(currentItems ())
            this.RestoreItems(redoStack.Pop())
            lastEditTicks <- 0L
            true
    member _.SelectionChanged = selectionChanged.Publish
    member _.ItemChanged = itemChanged.Publish
    member _.DirtyChanged = dirtyChanged.Publish

    member _.IsDirty = dirty
    member _.MarkSaved() = setDirty false
    member _.MarkDirty() = setDirty true

    member _.ProjectPath
        with get () = projectPath
        and set v = projectPath <- v

    member _.PlcIp
        with get () = plcIp
        and set v =
            plcIp <- v
            setDirty true

    member _.Port
        with get () = port
        and set v =
            port <- v
            setDirty true

    member _.CycleMs
        with get () = cycleMs
        and set v =
            cycleMs <- v
            setDirty true

    member _.CopyBuffer = copyBuffer

    // ---------- 선택 ----------

    member _.Selection = selection :> seq<ElementVm>
    member _.SelectionCount = selection.Count

    member _.Primary =
        if selection.Count = 0 then None else Some selection.[selection.Count - 1]

    member _.IsSelected(vm: ElementVm) = selection.Contains vm

    member _.Select(vm: ElementVm option, additive: bool) =
        if not additive then selection.Clear()
        match vm with
        | Some v ->
            if additive && selection.Contains v then selection.Remove v |> ignore
            else selection.Add v
        | None -> ()
        selectionChanged.Trigger()

    member _.SelectMany(items: ElementVm seq) =
        selection.Clear()
        for i in items do
            selection.Add i
        selectionChanged.Trigger()

    member this.SelectAll() = this.SelectMany elements

    member _.ClearSelection() =
        if selection.Count > 0 then
            selection.Clear()
            selectionChanged.Trigger()

    member _.FindById(id: string) =
        elements |> Seq.tryFind (fun e -> e.Id = id)

    // ---------- 프로젝트 입출력 ----------

    member this.LoadProject(p: HmiProject, path: string) =
        undoStack.Clear()
        redoStack.Clear()
        historyChanged.Trigger()
        screenWidth <- p.ScreenWidth
        screenHeight <- p.ScreenHeight
        selection.Clear()
        elements.Clear()
        for item in p.Items do
            elements.Add(attach (ElementVm item))
        plcIp <- p.PlcIp
        port <- p.Port
        cycleMs <- p.CycleMs
        projectPath <- path
        copyBuffer <- []
        pasteSerial <- 0
        setDirty false
        structureChanged.Trigger()
        selectionChanged.Trigger()
        screenChanged.Trigger()

    member _.ToProject() : HmiProject =
        { PlcIp = plcIp
          Port = port
          CycleMs = cycleMs
          Items = elements |> Seq.map (fun e -> Item.normalize (e.ToItem())) |> List.ofSeq
          ScreenWidth = screenWidth
          ScreenHeight = screenHeight }
        |> Project.fitScreen

    /// 전체 요소 검사. 첫 오류 메시지를 돌려준다. (원본 ValidateItem 규칙)
    member _.Validate() : Result<unit, string> =
        let mutable error = None
        for e in elements do
            if error.IsNone then
                match Item.validate (Item.normalize (e.ToItem())) with
                | Error m -> error <- Some m
                | Ok() -> ()
        match error with
        | Some m -> Error m
        | None -> Ok()

    member _.ScanAddresses() =
        Project.scanAddresses (elements |> Seq.map (fun e -> e.ToItem()))

    // ---------- 요소 편집 ----------

    member private _.AddItem(item: HmiItem, select: bool) =
        let vm = attach (ElementVm item)
        elements.Add vm
        if select then selection.Add vm
        setDirty true
        vm

    member this.AddNew(kind: ItemKind) =
        pushHistory 0L
        selection.Clear()
        // 같은 종류가 이미 화면에 있으면 그 크기를 그대로 물려받는다.
        // (한 번 알맞게 키워 두면 다음부터 그 크기가 기본이 된다)
        let item =
            match elements |> Seq.filter (fun e -> e.Kind = kind) |> Seq.tryLast with
            | Some prev -> { Item.create kind with Width = prev.Width; Height = prev.Height }
            | None -> Item.create kind
        let vm = this.AddItem(item, true)
        structureChanged.Trigger()
        selectionChanged.Trigger()
        vm

    /// 원본 AddMultipleSwitches: M주소 자동 증가 + 5열 자동 배치
    member this.AddSwitches(count: int) =
        pushHistory 0L
        let startM = Project.nextMAddress (elements |> Seq.map (fun e -> e.ToItem()))
        let baseIndex = elements.Count
        selection.Clear()
        for i in 0 .. count - 1 do
            let px, py = Project.nextFreePosition (baseIndex + i)
            let item =
                { Item.create Switch with
                    Name = sprintf "%s %d" (I18n.t "type.switch") (i + 1)
                    Device = "M" + string (startM + i)
                    Action = Toggle
                    X = px
                    Y = py
                    Width = 180
                    Height = 100 }
            this.AddItem(item, true) |> ignore
        structureChanged.Trigger()
        selectionChanged.Trigger()
        startM

    member _.CopySelection() =
        if selection.Count > 0 then
            copyBuffer <- selection |> Seq.map (fun e -> Item.clone false (e.ToItem())) |> List.ofSeq
            pasteSerial <- 0
        selection.Count

    /// 원본 PasteEditorRows: 붙여넣을 때마다 20px 씩 어긋나게
    member this.PasteOffset() =
        pushHistory 0L
        if copyBuffer.IsEmpty then 0
        else
            pasteSerial <- pasteSerial + 1
            let offset = 20 * pasteSerial
            selection.Clear()
            for src in copyBuffer do
                let item =
                    { Item.clone true src with
                        X = max 0 (src.X + offset)
                        Y = max 0 (src.Y + offset) }
                this.AddItem(item, true) |> ignore
            structureChanged.Trigger()
            selectionChanged.Trigger()
            copyBuffer.Length

    /// 원본 PasteToCanvas: 마우스 위치에 상대 배치를 유지한 채 붙여넣기
    member this.PasteAt(px: int, py: int) =
        pushHistory 0L
        if copyBuffer.IsEmpty then 0
        else
            let minX = copyBuffer |> List.map (fun i -> i.X) |> List.min
            let minY = copyBuffer |> List.map (fun i -> i.Y) |> List.min
            selection.Clear()
            for src in copyBuffer do
                let item =
                    { Item.clone true src with
                        X = max 0 (px + (src.X - minX))
                        Y = max 0 (py + (src.Y - minY)) }
                this.AddItem(item, true) |> ignore
            structureChanged.Trigger()
            selectionChanged.Trigger()
            copyBuffer.Length

    /// 원본 DuplicateSelectedEditorRows: 선택 묶음을 N벌 복제
    member this.DuplicateSelection(count: int) =
        pushHistory 0L
        let originals = selection |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
        if originals.IsEmpty || count <= 0 then 0
        else
            selection.Clear()
            for k in 1..count do
                let offset = 20 * k
                for src in originals do
                    let item =
                        { Item.clone true src with
                            X = max 0 (src.X + offset)
                            Y = max 0 (src.Y + offset) }
                    this.AddItem(item, true) |> ignore
            structureChanged.Trigger()
            selectionChanged.Trigger()
            originals.Length * count

    member this.CopyOne(vm: ElementVm) =
        copyBuffer <- [ Item.clone false (vm.ToItem()) ]
        pasteSerial <- 0

    member this.DuplicateOne(vm: ElementVm) =
        pushHistory 0L
        let src = vm.ToItem()
        let item = { Item.clone true src with X = src.X + 20; Y = src.Y + 20 }
        selection.Clear()
        let created = this.AddItem(item, true)
        structureChanged.Trigger()
        selectionChanged.Trigger()
        created

    member this.Remove(items: ElementVm seq) =
        pushHistory 0L
        let doomed = List.ofSeq items
        for vm in doomed do
            elements.Remove vm |> ignore
            selection.Remove vm |> ignore
        if not doomed.IsEmpty then
            setDirty true
            structureChanged.Trigger()
            selectionChanged.Trigger()
        doomed.Length

    member this.RemoveSelected() = this.Remove(List.ofSeq selection)

    // ---------- 정렬 ----------

    /// 계산 결과(위치/크기)를 요소들에 적용한다.
    member private _.ApplyLayout(updated: HmiItem list) =
        let mutable changed = 0
        for item in updated do
            match elements |> Seq.tryFind (fun e -> e.Id = item.Id) with
            | Some vm ->
                if vm.X <> item.X || vm.Y <> item.Y || vm.Width <> item.Width || vm.Height <> item.Height then
                    vm.SetBounds(item.X, item.Y, item.Width, item.Height)
                    changed <- changed + 1
            | None -> ()
        changed

    /// 전체(또는 선택한) 요소를 격자로 자동 배치한다.
    member this.AutoArrange(canvasWidth: int) =
        pushHistory 0L
        let targets =
            if selection.Count >= 2 then selection |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
            else elements |> Seq.filter (fun e -> e.Enabled) |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
        if targets.IsEmpty then 0
        else this.ApplyLayout(Layout.autoArrange canvasWidth targets)

    /// 선택한 요소를 기준선에 맞춘다.
    member this.AlignSelection(mode: AlignMode) =
        pushHistory 0L
        let targets = selection |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
        if targets.Length < 2 then 0 else this.ApplyLayout(Layout.align mode targets)

    /// 선택한 요소의 간격을 균등하게 만든다.
    member this.DistributeSelection(horizontal: bool) =
        pushHistory 0L
        let targets = selection |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
        if targets.Length < 3 then 0 else this.ApplyLayout(Layout.distribute horizontal targets)

    /// 마지막으로 고른 요소의 크기에 나머지를 맞춘다.
    member this.MatchSelectionSize() =
        pushHistory 0L
        let targets = selection |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
        match this.Primary with
        | Some primary when targets.Length >= 2 -> this.ApplyLayout(Layout.matchSize (primary.ToItem()) targets)
        | _ -> 0

    /// 요소를 맨 앞으로 (캔버스 Z 순서)
    member _.MoveToFront(vm: ElementVm) =
        let idx = elements.IndexOf vm
        if idx >= 0 && idx < elements.Count - 1 then
            elements.Move(idx, elements.Count - 1)
            structureChanged.Trigger()
