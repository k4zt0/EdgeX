namespace XgbHmi.App.ViewModels

open System
open System.Collections.ObjectModel
open System.Collections.Specialized
open XgbHmi.Core

/// 되돌리기 스냅숏 한 벌. 화면 요소와 터치스크린 부품을 함께 담아 Ctrl+Z 가 양쪽에 통한다.
type internal EditSnapshot =
    { Items: HmiItem list
      Parts: HmiPart list
      HmiWidth: int
      HmiHeight: int
      HmiBackground: string }

/// 프로젝트 편집 상태 전체. 표 / 트리 / 캔버스 / 속성창 / HMI 작화가 이 하나를 공유한다.
type AppState() =

    let elements = ObservableCollection<ElementVm>()
    let selection = ResizeArray<ElementVm>()

    let structureChanged = Event<unit>()
    let selectionChanged = Event<unit>()
    let itemChanged = Event<ElementVm * string>()
    let dirtyChanged = Event<bool>()

    // ---------- 터치스크린(HMI) 작화 ----------
    let hmiParts = ObservableCollection<HmiPartVm>()
    let mutable hmiSelected: HmiPartVm option = None
    let mutable hmiWidth = HmiLimits.defaultWidth
    let mutable hmiHeight = HmiLimits.defaultHeight
    let mutable hmiBackground = ""
    let mutable hmiCopyBuffer: HmiPart list = []

    let hmiStructureChanged = Event<unit>()
    let hmiSelectionChanged = Event<unit>()
    let hmiPartChanged = Event<HmiPartVm * string>()
    let hmiScreenChanged = Event<unit>()

    let mutable projectPath = ""
    /// 붙일 PLC 목록. 이더넷·RS-232C·RS-485 를 섞어 여러 대를 쓸 수 있다.
    let mutable plcs: PlcLink list = [ PlcLink.empty ]
    let mutable cycleMs = Limits.defaultCycleMs
    let mutable copyBuffer: HmiItem list = []
    let mutable pasteSerial = 0
    let mutable dirty = false
    let mutable screenWidth = Limits.defaultScreenWidth
    let mutable screenHeight = Limits.defaultScreenHeight

    let screenChanged = Event<unit>()
    let historyChanged = Event<unit>()
    let plcsChanged = Event<unit>()

    // 되돌리기 / 다시 실행 (요소 목록 전체를 스냅숏으로 보관한다)
    let undoStack = System.Collections.Generic.Stack<EditSnapshot>()
    let redoStack = System.Collections.Generic.Stack<EditSnapshot>()
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

    let currentParts () =
        hmiParts |> Seq.map (fun e -> e.ToPart()) |> List.ofSeq

    let snapshot () =
        { Items = currentItems ()
          Parts = currentParts ()
          HmiWidth = hmiWidth
          HmiHeight = hmiHeight
          HmiBackground = hmiBackground }

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
                undoStack.Push(snapshot ())
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

    let attachPart (vm: HmiPartVm) =
        vm.SetBeforeChangeHook(fun () -> pushHistory 700L)
        vm.PropertyChangedEvent.Add(fun args ->
            setDirty true
            hmiPartChanged.Trigger(vm, args.PropertyName))
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

    member private _.RestoreSnapshot(snap: EditSnapshot) =
        restoring <- true
        selection.Clear()
        elements.Clear()
        for item in snap.Items do
            elements.Add(attach (ElementVm item))
        hmiSelected <- None
        hmiParts.Clear()
        for part in snap.Parts do
            hmiParts.Add(attachPart (HmiPartVm part))
        hmiWidth <- snap.HmiWidth
        hmiHeight <- snap.HmiHeight
        hmiBackground <- snap.HmiBackground
        restoring <- false
        setDirty true
        structureChanged.Trigger()
        selectionChanged.Trigger()
        hmiScreenChanged.Trigger()
        hmiStructureChanged.Trigger()
        hmiSelectionChanged.Trigger()
        historyChanged.Trigger()

    member this.Undo() =
        if undoStack.Count = 0 then false
        else
            redoStack.Push(snapshot ())
            this.RestoreSnapshot(undoStack.Pop())
            lastEditTicks <- 0L
            true

    member this.Redo() =
        if redoStack.Count = 0 then false
        else
            undoStack.Push(snapshot ())
            this.RestoreSnapshot(redoStack.Pop())
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

    // ---------- PLC 목록 ----------

    member _.Plcs = plcs
    member _.PlcsChanged = plcsChanged.Publish

    /// PLC 설정 대화상자에서 고친 목록을 받아 넣는다.
    /// 없어진 PLC 를 가리키던 요소는 첫 번째 PLC 로 옮긴다.
    member _.SetPlcs(list: PlcLink list) =
        let normalized = Project.normalizePlcs { Project.empty with Plcs = list }
        plcs <- normalized
        for e in elements do
            let resolved = Project.resolvePlcId normalized e.PlcId
            if e.PlcId <> resolved then e.PlcId <- resolved
        match normalized with
        | first :: _ -> cycleMs <- first.CycleMs
        | [] -> ()
        setDirty true
        plcsChanged.Trigger()

    /// 요소가 실제로 쓰는 PLC 이름표 (비어 있으면 첫 번째 PLC)
    member _.PlcIdOf(vm: ElementVm) = Project.resolvePlcId plcs vm.PlcId

    /// 첫 번째 PLC (기본 PLC)
    member _.PrimaryPlc =
        match plcs with
        | first :: _ -> first
        | [] -> PlcLink.empty

    /// 화면에 보여 줄 PLC 목록 요약 (툴바 버튼 / 상태 표시줄)
    member _.PlcSummary =
        match plcs with
        | [] -> ""
        | [ one ] -> PlcLink.endpoint one
        | many -> sprintf "%s + %d" (PlcLink.endpoint (List.head many)) (many.Length - 1)

    /// v6 호환용: 첫 이더넷 PLC 의 IP / 포트
    member _.PlcIp
        with get () =
            match plcs |> List.tryFind (fun l -> l.Kind = LinkEthernet) with
            | Some l -> l.Ip
            | None -> ""
        and set v =
            plcs <-
                plcs
                |> List.map (fun l -> if l.Kind = LinkEthernet && l.Ip <> v then { l with Ip = v } else l)
            setDirty true
            plcsChanged.Trigger()

    member _.Port
        with get () =
            match plcs |> List.tryFind (fun l -> l.Kind = LinkEthernet) with
            | Some l -> l.Port
            | None -> Limits.defaultPort
        and set v =
            plcs <-
                plcs
                |> List.map (fun l -> if l.Kind = LinkEthernet && l.Port <> v then { l with Port = v } else l)
            setDirty true
            plcsChanged.Trigger()

    /// 폴링 주기. 붙어 있는 PLC 전부에 적용한다.
    member _.CycleMs
        with get () = cycleMs
        and set v =
            cycleMs <- v
            plcs <- plcs |> List.map (fun l -> { l with CycleMs = v })
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
        hmiSelected <- None
        hmiParts.Clear()
        for part in p.Hmi.Parts do
            hmiParts.Add(attachPart (HmiPartVm part))
        hmiWidth <- p.Hmi.Width
        hmiHeight <- p.Hmi.Height
        hmiBackground <- p.Hmi.Background
        hmiCopyBuffer <- []
        plcs <- Project.normalizePlcs p
        cycleMs <- (match plcs with first :: _ -> first.CycleMs | [] -> p.CycleMs)
        projectPath <- path
        copyBuffer <- []
        pasteSerial <- 0
        setDirty false
        structureChanged.Trigger()
        selectionChanged.Trigger()
        screenChanged.Trigger()
        hmiScreenChanged.Trigger()
        hmiStructureChanged.Trigger()
        hmiSelectionChanged.Trigger()
        plcsChanged.Trigger()

    member this.ToProject() : HmiProject =
        { PlcIp = this.PlcIp
          Port = this.Port
          CycleMs = cycleMs
          Plcs = plcs
          Items = elements |> Seq.map (fun e -> Item.normalize (e.ToItem())) |> List.ofSeq
          ScreenWidth = screenWidth
          ScreenHeight = screenHeight
          Hmi =
            HmiScreen.normalize
                { Width = hmiWidth
                  Height = hmiHeight
                  Background = hmiBackground
                  Parts = currentParts () } }
        |> Project.normalizeLinks
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

    /// PLC 별 폴링 목록. 여러 대를 붙였을 때 회선마다 제 주소만 읽게 나눈다.
    member _.ScanPlan() =
        let items = elements |> Seq.map (fun e -> e.ToItem()) |> List.ofSeq
        let groups = Project.scanAddressesByPlc plcs items

        // 램프 배열의 연속 비트는 어느 요소에도 없으므로 그 요소가 쓰는 PLC 목록에 넣어 줘야 값이 들어온다.
        let extras = Collections.Generic.Dictionary<string, ResizeArray<string>>(StringComparer.OrdinalIgnoreCase)
        for part in hmiParts do
            if part.Kind = PartLampArray && part.Count > 1 then
                match elements |> Seq.tryFind (fun e -> e.Id = part.TargetId) with
                | Some target when not (String.IsNullOrWhiteSpace target.Device) ->
                    let id = Project.resolvePlcId plcs target.PlcId
                    if not (extras.ContainsKey id) then extras.[id] <- ResizeArray<string>()
                    for i in 1 .. part.Count - 1 do
                        try extras.[id].Add(XgbHmi.Protocol.Address.offsetBit target.Device i) with _ -> ()
                | _ -> ()

        groups
        |> List.map (fun (plcId, bits, words) ->
            let all = ResizeArray<string>(bits)
            let has (v: string) =
                all |> Seq.exists (fun s -> String.Equals(s, v, StringComparison.OrdinalIgnoreCase))
            (match extras.TryGetValue plcId with
             | true, list ->
                 for a in list do
                     if not (has a) then all.Add a
             | _ -> ())
            plcId, List.ofSeq all, words)

    // ---------- 요소 편집 ----------

    member private _.AddItem(item: HmiItem, select: bool) =
        // 새 요소가 어느 PLC 를 쓸지 처음부터 채워 둔다. (표/속성창에서 빈 칸으로 보이지 않게)
        let item = { item with PlcId = Project.resolvePlcId plcs item.PlcId }
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

    // ---------- 터치스크린(HMI) 작화 ----------

    member _.HmiParts = hmiParts
    member _.HmiStructureChanged = hmiStructureChanged.Publish
    member _.HmiSelectionChanged = hmiSelectionChanged.Publish
    member _.HmiPartChanged = hmiPartChanged.Publish
    member _.HmiScreenChanged = hmiScreenChanged.Publish

    /// 터치패널 가로 해상도
    member _.HmiWidth
        with get () = hmiWidth
        and set v =
            let v = max HmiLimits.minScreen (min HmiLimits.maxScreen v)
            if hmiWidth <> v then
                hmiWidth <- v
                setDirty true
                hmiScreenChanged.Trigger()

    member _.HmiHeight
        with get () = hmiHeight
        and set v =
            let v = max HmiLimits.minScreen (min HmiLimits.maxScreen v)
            if hmiHeight <> v then
                hmiHeight <- v
                setDirty true
                hmiScreenChanged.Trigger()

    member _.HmiBackground
        with get () = hmiBackground
        and set v =
            let v = HmiPart.normalizeColor v
            if hmiBackground <> v then
                hmiBackground <- v
                setDirty true
                hmiScreenChanged.Trigger()

    member _.HmiSelected = hmiSelected

    member _.IsPartSelected(vm: HmiPartVm) =
        match hmiSelected with
        | Some s -> Object.ReferenceEquals(s, vm)
        | None -> false

    member _.SelectPart(vm: HmiPartVm option) =
        let same =
            match hmiSelected, vm with
            | Some a, Some b -> Object.ReferenceEquals(a, b)
            | None, None -> true
            | _ -> false
        if not same then
            hmiSelected <- vm
            hmiSelectionChanged.Trigger()

    member _.FindPartById(id: string) =
        hmiParts |> Seq.tryFind (fun e -> e.Id = id)

    member private _.AddPartItem(part: HmiPart, select: bool) =
        let vm = attachPart (HmiPartVm part)
        hmiParts.Add vm
        if select then hmiSelected <- Some vm
        setDirty true
        vm

    /// 빈 자리를 찾아 새 부품을 놓는다.
    member this.AddPart(kind: HmiPartKind) =
        pushHistory 0L
        let template = HmiPart.create kind
        // 같은 종류가 이미 있으면 그 생김새(크기·모양·색·글자 크기)를 물려받는다.
        // 한 번 알맞게 꾸며 두면 다음 부품부터 그대로 나온다.
        let template =
            match hmiParts |> Seq.filter (fun e -> e.Kind = kind) |> Seq.tryLast with
            | Some prev ->
                let q = prev.ToPart()
                { template with
                    Width = q.Width
                    Height = q.Height
                    Shape = q.Shape
                    OffColor = q.OffColor
                    OnColor = q.OnColor
                    TextColor = q.TextColor
                    BorderColor = q.BorderColor
                    FontSize = q.FontSize
                    Corner = q.Corner
                    Align = q.Align }
            | None -> template
        let x, y = HmiScreen.nextFreePosition (currentParts ()) hmiWidth hmiHeight template.Width template.Height
        let vm = this.AddPartItem({ template with X = x; Y = y }, true)
        hmiStructureChanged.Trigger()
        hmiSelectionChanged.Trigger()
        vm

    /// 선택한 화면 요소를 그대로 터치스크린 부품으로 만든다.
    /// 스위치는 버튼, 램프는 램프, 숫자는 값 부품으로 짝지어 준다.
    member this.AddPartsFromElements(targets: ElementVm seq) =
        let targets = List.ofSeq targets
        if targets.IsEmpty then 0
        else
            pushHistory 0L
            hmiSelected <- None
            let mutable made = 0
            for t in targets do
                let kind =
                    match t.Kind with
                    | Switch -> Some PartButton
                    | SwitchLamp -> Some PartButton
                    | Lamp -> Some PartLamp
                    | NumInput -> Some PartValue
                    | NumDisplay -> Some PartValue
                    | Text -> Some PartLabel
                    | MasterSwitch -> None
                match kind with
                | None -> ()
                | Some kind ->
                    let template = HmiPart.create kind
                    let x, y =
                        HmiScreen.nextFreePosition (currentParts ()) hmiWidth hmiHeight template.Width template.Height
                    let part =
                        { template with
                            X = x
                            Y = y
                            TargetId = (if kind = PartLabel then "" else t.Id)
                            Text = (if kind = PartLabel then t.Name else "") }
                    this.AddPartItem(part, false) |> ignore
                    made <- made + 1
            if made > 0 then
                hmiStructureChanged.Trigger()
                hmiSelectionChanged.Trigger()
            made

    member this.DuplicatePart(vm: HmiPartVm) =
        pushHistory 0L
        let src = vm.ToPart()
        hmiSelected <- None
        let created = this.AddPartItem({ HmiPart.clone true src with X = src.X + 24; Y = src.Y + 24 }, true)
        hmiStructureChanged.Trigger()
        hmiSelectionChanged.Trigger()
        created

    member _.CopyPart(vm: HmiPartVm) =
        hmiCopyBuffer <- [ HmiPart.clone false (vm.ToPart()) ]

    member this.PastePartAt(px: int, py: int) =
        if hmiCopyBuffer.IsEmpty then 0
        else
            pushHistory 0L
            hmiSelected <- None
            for src in hmiCopyBuffer do
                this.AddPartItem({ HmiPart.clone true src with X = max 0 px; Y = max 0 py }, true) |> ignore
            hmiStructureChanged.Trigger()
            hmiSelectionChanged.Trigger()
            hmiCopyBuffer.Length

    member this.RemovePart(vm: HmiPartVm) =
        pushHistory 0L
        if hmiParts.Remove vm then
            if this.IsPartSelected vm then hmiSelected <- None
            setDirty true
            hmiStructureChanged.Trigger()
            hmiSelectionChanged.Trigger()
            true
        else false

    /// 부품을 맨 앞으로 (겹쳤을 때 그리는 순서)
    member _.MovePartToFront(vm: HmiPartVm) =
        let idx = hmiParts.IndexOf vm
        if idx >= 0 && idx < hmiParts.Count - 1 then
            pushHistory 0L
            hmiParts.Move(idx, hmiParts.Count - 1)
            setDirty true
            hmiStructureChanged.Trigger()

    /// 부품을 맨 뒤로 (패널을 배경으로 깔 때)
    member _.MovePartToBack(vm: HmiPartVm) =
        let idx = hmiParts.IndexOf vm
        if idx > 0 then
            pushHistory 0L
            hmiParts.Move(idx, 0)
            setDirty true
            hmiStructureChanged.Trigger()
