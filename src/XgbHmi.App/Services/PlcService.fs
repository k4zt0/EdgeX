namespace XgbHmi.App.Services

open System
open System.Collections.Generic
open System.Text
open System.Threading
open System.Threading.Tasks
open XgbHmi.Core
open XgbHmi.Protocol

type LogLevel =
    | Info
    | Success
    | Warn
    | Failure
    /// 프레임 단위 통신 추적 (TX/RX)
    | Trace

type ConnState =
    | Disconnected
    | Connecting
    | Online
    | Faulted

/// PLC 한 대가 한 주기에 읽어야 할 주소 목록
type PlcScan =
    { PlcId: string
      Bits: string list
      Words: string list }

/// 상태 표시줄 / 트리에 보여 줄 PLC 한 대의 지금 상태
type PlcLinkStatus =
    { Config: PlcLink
      State: ConnState
      Detail: string
      ProfileName: string
      CycleMs: float
      Frames: int64
      Errors: int64 }

/// PLC 한 대의 연결 / 폴링 상태
type private LinkRuntime(config: PlcLink, client: IPlcLink) =
    member val Config = config with get, set
    member _.Client = client
    /// XgtClient / CnetClient 는 스레드 안전하지 않으므로 이 회선의 모든 접근을 이 락으로 직렬화한다.
    member val Sync = obj () with get
    member val Worker: Thread = null with get, set
    member val State = Connecting with get, set
    member val Detail = "" with get, set
    member val CycleCount = 0L with get, set
    member val LastCycleMs = 0.0 with get, set

/// PLC 연결 / 주기 폴링 / 쓰기를 담당한다.
/// 이더넷(FEnet)·RS-232C·RS-485(Cnet) 를 섞어 여러 대를 동시에 붙일 수 있고,
/// 회선마다 제 스레드로 폴링한다. RS-485 는 한 회선을 여러 국번이 나눠 쓰므로 회선 안에서 자동으로 한 줄로 세운다.
type PlcService() =

    let cacheSync = obj ()
    let linksSync = obj ()

    /// 키는 "PLC1|M01008" 처럼 PLC 이름표를 앞에 붙인다. 여러 대가 같은 주소를 써도 섞이지 않는다.
    let bitCache = Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase)
    let wordCache = Dictionary<string, uint16>(StringComparer.OrdinalIgnoreCase)

    let logEvent = Event<LogLevel * string>()
    let stateEvent = Event<ConnState * string>()
    let valuesEvent = Event<unit>()

    let links = ResizeArray<LinkRuntime>()
    let mutable running = false
    let mutable cycleMs = Limits.defaultCycleMs
    let mutable scanPlan: unit -> PlcScan list = fun () -> []
    let mutable traceEnabled = false
    let mutable logChanges = true

    let log level (message: string) = logEvent.Trigger(level, message)

    let cacheKey (plcId: string) (address: string) =
        (if String.IsNullOrWhiteSpace plcId then "" else plcId.Trim()) + "|" + (if isNull address then "" else address.Trim())

    let snapshot () = lock linksSync (fun () -> List.ofSeq links)

    let defaultLink () =
        match snapshot () with
        | first :: _ -> Some first
        | [] -> None

    let findLink (plcId: string) =
        let all = snapshot ()
        match all |> List.tryFind (fun l -> String.Equals(l.Config.Id, plcId, StringComparison.OrdinalIgnoreCase)) with
        | Some l -> Some l
        | None ->
            match all with
            | first :: _ -> Some first
            | [] -> None

    /// 여러 대를 붙였을 때만 로그 앞에 [PLC2] 를 붙인다. 한 대일 때는 예전과 같은 줄이 나온다.
    let tagOf (link: LinkRuntime) =
        if (snapshot ()).Length > 1 then "[" + link.Config.Id + "] " else ""

    /// 통신 창구의 추적을 화면 로그로 넘긴다.
    let attachTrace (link: LinkRuntime) =
        link.Client.TraceEnabled <- traceEnabled
        link.Client.Trace.Add(fun t ->
            let tag =
                match t.Kind with
                | Tx -> "TX  "
                | Rx -> "RX  "
                | Note -> "··  "
            let prefix = tag + tagOf link
            let line =
                if String.IsNullOrEmpty t.Hex then prefix + t.Summary
                else prefix + t.Summary + "\n      " + t.Hex
            logEvent.Trigger(Trace, line))

    let setCacheBits (link: LinkRuntime) (values: Dictionary<string, bool>) =
        let changes = ResizeArray<string>()
        lock cacheSync (fun () ->
            for kv in values do
                let key = cacheKey link.Config.Id kv.Key
                match bitCache.TryGetValue key with
                | true, previous when previous <> kv.Value ->
                    changes.Add(sprintf "%s%s : %s -> %s" (tagOf link) kv.Key (if previous then "ON" else "OFF") (if kv.Value then "ON" else "OFF"))
                | _ -> ()
                bitCache.[key] <- kv.Value)
        if logChanges then
            for c in changes do
                logEvent.Trigger(Info, "CHANGE " + c)

    let setCacheWords (link: LinkRuntime) (values: Dictionary<string, uint16>) =
        let changes = ResizeArray<string>()
        lock cacheSync (fun () ->
            for kv in values do
                let key = cacheKey link.Config.Id kv.Key
                match wordCache.TryGetValue key with
                | true, previous when previous <> kv.Value ->
                    changes.Add(sprintf "%s%s : %d -> %d (0x%04X, signed %d)" (tagOf link) kv.Key previous kv.Value kv.Value (int16 kv.Value))
                | _ -> ()
                wordCache.[key] <- kv.Value)
        if logChanges then
            for c in changes do
                logEvent.Trigger(Info, "CHANGE " + c)

    /// 이 회선의 통신 창구를 만든다. 이더넷은 FEnet, 직렬은 Cnet.
    let createClient (config: PlcLink) : IPlcLink =
        match config.Kind with
        | LinkEthernet -> new XgtClient(config.Ip, config.Port, 1800) :> IPlcLink
        | LinkRs232
        | LinkRs485 ->
            let settings =
                { PortName = config.SerialPort
                  Baud = config.Baud
                  DataBits = config.DataBits
                  Parity = CnetParity.parse config.Parity
                  StopBits = config.StopBits
                  Station = config.Station
                  TimeoutMs = 1800
                  HalfDuplex = (config.Kind = LinkRs485) }
            new CnetClient(settings) :> IPlcLink

    let kindLabel (config: PlcLink) =
        match config.Kind with
        | LinkEthernet -> "이더넷 FEnet"
        | LinkRs232 -> "RS-232C Cnet"
        | LinkRs485 -> "RS-485 Cnet"

    /// 회선 전체를 하나의 상태로 묶어 상태 표시줄에 올린다.
    let publishState () =
        let all = snapshot ()
        if all.IsEmpty then stateEvent.Trigger(Disconnected, "")
        else
            let faulted = all |> List.filter (fun l -> l.State = Faulted)
            let time = DateTime.Now.ToString "HH:mm:ss.fff"
            if faulted.Length = all.Length then
                let head = List.head faulted
                stateEvent.Trigger(Faulted, (if all.Length > 1 then tagOf head else "") + head.Detail)
            elif all.Length = 1 then
                // 한 대만 붙였으면 예전과 같은 문장을 그대로 보여 준다.
                stateEvent.Trigger(Online, (List.head all).Detail)
            else
                let summary =
                    all
                    |> List.map (fun l ->
                        sprintf "%s %s" l.Config.Id (if l.State = Faulted then "✖" else sprintf "%.0f ms" l.LastCycleMs))
                    |> String.concat "  ·  "
                let warn = if faulted.IsEmpty then "" else sprintf "   (%d/%d 오류)" faulted.Length all.Length
                stateEvent.Trigger(Online, sprintf "%s   %s%s" time summary warn)

    /// 회선 한 개의 폴링 고리. 회선마다 제 스레드에서 돈다.
    let workerLoop (link: LinkRuntime) () =
        while running do
            let sw = Diagnostics.Stopwatch.StartNew()
            try
                let plan =
                    scanPlan ()
                    |> List.tryFind (fun s -> String.Equals(s.PlcId, link.Config.Id, StringComparison.OrdinalIgnoreCase))
                let bits, words =
                    match plan with
                    | Some p -> p.Bits, p.Words
                    | None -> [], []

                let bvals = Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase)
                let bitArray = List.toArray bits
                let mutable i = 0
                let mutable aborted = false
                while not aborted && i < bitArray.Length do
                    let chunk = bitArray.[i .. min (i + 15) (bitArray.Length - 1)]
                    let part =
                        lock link.Sync (fun () -> if running then link.Client.ReadBits chunk else null)
                    if isNull part then aborted <- true
                    else
                        for kv in part do
                            bvals.[kv.Key] <- kv.Value
                        i <- i + 16

                if not aborted then
                    let wvals = Dictionary<string, uint16>(StringComparer.OrdinalIgnoreCase)
                    for d in words do
                        if not aborted then
                            let v = lock link.Sync (fun () -> if running then Some(link.Client.ReadWord d) else None)
                            match v with
                            | None -> aborted <- true
                            | Some value -> wvals.[d] <- value

                    if not aborted then
                        setCacheBits link bvals
                        setCacheWords link wvals
                        sw.Stop()
                        link.LastCycleMs <- sw.Elapsed.TotalMilliseconds
                        link.CycleCount <- link.CycleCount + 1L
                        link.State <- Online
                        link.Detail <-
                            sprintf
                                "%s   비트 %d · WORD %d   스캔 %.0f ms"
                                (DateTime.Now.ToString "HH:mm:ss.fff")
                                bitArray.Length
                                (List.length words)
                                link.LastCycleMs
                        if running then
                            valuesEvent.Trigger()
                            publishState ()
                            if traceEnabled then
                                logEvent.Trigger(
                                    Trace,
                                    sprintf
                                        "SCAN %s#%d  비트 %d개(프레임 %d) · WORD %d개  →  %.1f ms"
                                        (tagOf link)
                                        link.CycleCount
                                        bitArray.Length
                                        ((bitArray.Length + 15) / 16)
                                        (List.length words)
                                        link.LastCycleMs)
            with ex ->
                sw.Stop()
                link.State <- Faulted
                link.Detail <- ex.Message
                publishState ()
                log Failure (sprintf "%sREAD ERROR (%.0f ms, %s): %s" (tagOf link) sw.Elapsed.TotalMilliseconds (ex.GetType().Name) ex.Message)
                Thread.Sleep 700

            Thread.Sleep(max Limits.minCycleMs link.Config.CycleMs)

    let disposeLinks () =
        let all = lock linksSync (fun () ->
            let copy = List.ofSeq links
            links.Clear()
            copy)
        for link in all do
            lock link.Sync (fun () -> (try link.Client.Dispose() with _ -> ()))

    member _.Log = logEvent.Publish
    member _.StateChanged = stateEvent.Publish
    member _.ValuesChanged = valuesEvent.Publish

    member _.IsRunning = running

    /// 붙어 있는 PLC 목록 (상태 표시줄 / 트리용)
    member _.LinkStatus =
        snapshot ()
        |> List.map (fun l ->
            { Config = l.Config
              State = l.State
              Detail = l.Detail
              ProfileName = l.Client.ProfileName
              CycleMs = l.LastCycleMs
              Frames = l.Client.FrameCount
              Errors = l.Client.ErrorCount })

    /// 요소가 PLC 를 고르지 않았을 때 쓰는 기본 PLC
    member _.DefaultPlcId =
        match defaultLink () with
        | Some l -> l.Config.Id
        | None -> ""

    member _.ProfileName =
        match snapshot () with
        | [] -> ""
        | [ one ] -> one.Client.ProfileName
        | many -> many |> List.map (fun l -> l.Config.Id + " " + l.Client.ProfileName) |> String.concat "  ·  "

    /// 그 PLC 가 지금 통신 오류인지. 카드를 빨간색으로 점등할지 정하는 데 쓴다.
    member _.IsFaulted(plcId: string) =
        match findLink plcId with
        | Some l -> l.State = Faulted
        | None -> false

    /// 한 대라도 통신 오류인지
    member _.AnyFaulted = snapshot () |> List.exists (fun l -> l.State = Faulted)

    member _.CycleMs
        with get () = cycleMs
        and set v =
            cycleMs <- max Limits.minCycleMs (min Limits.maxCycleMs v)
            // 툴바에서 주기를 바꾸면 붙어 있는 회선 전부에 적용한다.
            for l in snapshot () do
                l.Config <- { l.Config with CycleMs = cycleMs }

    /// TX/RX 프레임 원문까지 출력 창에 남길지
    member _.TraceEnabled
        with get () = traceEnabled
        and set v =
            traceEnabled <- v
            for l in snapshot () do
                l.Client.TraceEnabled <- v

    /// 값이 바뀔 때마다 출력 창에 남길지
    member _.LogChanges
        with get () = logChanges
        and set v = logChanges <- v

    /// 마지막 스캔에 걸린 시간(ms). 여러 대면 가장 오래 걸린 회선.
    member _.LastCycleMs =
        match snapshot () with
        | [] -> 0.0
        | all -> all |> List.map (fun l -> l.LastCycleMs) |> List.max

    member _.CycleCount = snapshot () |> List.sumBy (fun l -> l.CycleCount)
    member _.FrameCount = snapshot () |> List.sumBy (fun l -> l.Client.FrameCount)
    member _.ErrorCount = snapshot () |> List.sumBy (fun l -> l.Client.ErrorCount)

    /// PLC 별 폴링 목록을 돌려주는 함수
    member _.SetScanPlan(provider: unit -> PlcScan list) = scanPlan <- provider

    /// 한 대만 쓸 때의 간단한 형태 (비트 목록, WORD 목록)
    member this.SetScanProvider(provider: unit -> string list * string list) =
        scanPlan <-
            fun () ->
                let bits, words = provider ()
                [ { PlcId = this.DefaultPlcId; Bits = bits; Words = words } ]

    /// 이름표가 비었거나 없어진 PLC 를 가리키면 첫 번째 PLC 로 본다. (요소가 PLC 를 고르지 않은 경우)
    member private _.ResolveId(plcId: string) =
        match findLink plcId with
        | Some l -> l.Config.Id
        | None -> (if isNull plcId then "" else plcId)

    member this.TryBit(plcId: string, address: string) =
        let key = cacheKey (this.ResolveId plcId) address
        lock cacheSync (fun () ->
            match bitCache.TryGetValue key with
            | true, v -> Some v
            | _ -> None)

    member this.TryWord(plcId: string, address: string) =
        let key = cacheKey (this.ResolveId plcId) address
        lock cacheSync (fun () ->
            match wordCache.TryGetValue key with
            | true, v -> Some v
            | _ -> None)

    member this.TryBit(address: string) = this.TryBit(this.DefaultPlcId, address)
    member this.TryWord(address: string) = this.TryWord(this.DefaultPlcId, address)

    /// PLC 여러 대를 한꺼번에 붙인다. 한 대라도 붙으면 운전을 시작하고,
    /// 못 붙은 회선은 오류로 남겨 상태 표시줄과 출력 창에 보여 준다.
    member this.Connect(configs: PlcLink list) : Result<string, string> =
        this.Disconnect()
        let wanted = configs |> List.map PlcLink.normalize |> List.filter (fun l -> l.Enabled)
        // 붙기 전에 막을 것: 쓸 PLC 가 하나도 없거나, 같은 회선에 국번이 겹치거나 통신 속도가 어긋나는 경우.
        // (한 회선에 두 대가 같은 국번으로 있으면 어느 PLC 가 답했는지 알 수 없다)
        let guard =
            if wanted.IsEmpty then
                Some "연결할 PLC 가 없습니다. PLC 설정에서 적어도 한 대는 '사용' 으로 두십시오."
            else
                match Project.validatePlcs wanted with
                | Error message -> Some message
                | Ok() -> None
        match guard with
        | Some message ->
            stateEvent.Trigger(Faulted, message)
            log Failure ("CONNECT ERROR: " + message)
            Error message
        | None ->
            stateEvent.Trigger(Connecting, "")
            let failures = ResizeArray<string>()
            let connected = ResizeArray<LinkRuntime>()

            for config in wanted do
                match PlcLink.validate config with
                | Error message ->
                    failures.Add(sprintf "[%s] %s" config.Id message)
                    log Failure (sprintf "[%s] CONNECT ERROR: %s" config.Id message)
                | Ok() ->
                    let client = createClient config
                    let link = LinkRuntime(config, client)
                    lock linksSync (fun () -> links.Add link)
                    attachTrace link
                    log Info (
                        sprintf
                            "[%s] CONNECT %s %s (타임아웃 1800 ms, 주기 %d ms)"
                            config.Id
                            (kindLabel config)
                            (PlcLink.endpoint config)
                            config.CycleMs)
                    try
                        client.Connect()
                        link.State <- Online
                        link.Detail <- DateTime.Now.ToString "HH:mm:ss.fff"
                        // 원본과 동일하게 P00000 을 한 번 읽어 캐시를 채운다.
                        let probe = lock link.Sync (fun () -> client.ReadBits [| "P00000" |])
                        setCacheBits link probe
                        connected.Add link
                        log Success (
                            sprintf
                                "[%s] CONNECTED %s  통신 조합=%s  프레임 %d개 교환"
                                config.Id
                                (PlcLink.endpoint config)
                                client.ProfileName
                                client.FrameCount)
                        let negotiation = client.NegotiationLog.TrimEnd()
                        if not (String.IsNullOrWhiteSpace negotiation) then log Info negotiation
                    with ex ->
                        link.State <- Faulted
                        link.Detail <- ex.Message
                        let diag = (try client.NegotiationLog with _ -> "")
                        (try client.Dispose() with _ -> ())
                        lock linksSync (fun () -> links.Remove link |> ignore)
                        failures.Add(sprintf "[%s] %s" config.Id ex.Message)
                        log Failure (sprintf "[%s] CONNECT/PLC ERROR: %s" config.Id ex.Message)
                        if not (String.IsNullOrWhiteSpace diag) then log Info (diag.TrimEnd())

            if connected.Count = 0 then
                disposeLinks ()
                running <- false
                let message = String.Join("\n", failures)
                stateEvent.Trigger(Faulted, (if failures.Count > 0 then failures.[0] else "연결 실패"))
                Error message
            else
                running <- true
                for link in connected do
                    let worker = Thread(ThreadStart(workerLoop link), IsBackground = true)
                    link.Worker <- worker
                    worker.Start()

                publishState ()
                if failures.Count > 0 then
                    log Warn (sprintf "PLC %d대 중 %d대만 연결되었습니다. 나머지는 계속 오류로 표시됩니다." wanted.Length connected.Count)

                let summary =
                    connected
                    |> Seq.map (fun l -> l.Config.Id + " " + l.Client.ProfileName)
                    |> String.concat "  ·  "
                Ok summary

    /// 이더넷 한 대만 붙이는 짧은 형태
    member this.Connect(ip: string, port: int, cycle: int) : Result<string, string> =
        this.Connect [ { PlcLink.empty with Ip = ip; Port = port; CycleMs = cycle } ]

    member _.Disconnect() =
        let all = snapshot ()
        if running && not all.IsEmpty then
            log Info (
                sprintf
                    "DISCONNECT 요청 — 스캔 %d회, 프레임 %d개, 오류 %d개"
                    (all |> List.sumBy (fun l -> l.CycleCount))
                    (all |> List.sumBy (fun l -> l.Client.FrameCount))
                    (all |> List.sumBy (fun l -> l.Client.ErrorCount)))
        running <- false
        disposeLinks ()
        lock cacheSync (fun () ->
            bitCache.Clear()
            wordCache.Clear())
        stateEvent.Trigger(Disconnected, "")
        log Info "Disconnected"

    /// 그 PLC 에서 실제로 통신을 한 번 한다. 회선이 없으면 오류를 돌려준다.
    member private _.WithLink(plcId: string, work: LinkRuntime -> 'T) : Result<'T, string> =
        match findLink plcId with
        | None -> Error "PLC 연결이 없습니다."
        | Some link ->
            try Ok(lock link.Sync (fun () -> work link))
            with ex -> Error ex.Message

    /// 토글용: 화면 캐시가 아니라 PLC의 실제 비트를 즉시 읽는다. (v4 토글 수정)
    member this.ReadBitNow(plcId: string, address: string) : Result<bool, string> =
        match this.WithLink(plcId, (fun link -> link.Config.Id, link.Client.ReadBits [| address |])) with
        | Error m -> Error m
        | Ok(id, fresh) ->
            match fresh.TryGetValue address with
            | true, v ->
                lock cacheSync (fun () -> bitCache.[cacheKey id address] <- v)
                Ok v
            | _ -> Error(address + " 현재값 읽기 응답이 없습니다.")

    member this.ReadBitNow(address: string) = this.ReadBitNow(this.DefaultPlcId, address)

    /// 비트 쓰기 + 30ms 후 읽기 확인 (원본 WriteBitForItem 과 동일한 절차)
    member this.WriteBitVerified(plcId: string, address: string, value: bool) : Result<bool option, string> =
        let sw = Diagnostics.Stopwatch.StartNew()
        let result =
            this.WithLink(
                plcId,
                fun link ->
                    link.Client.WriteBit(address, value)
                    Thread.Sleep 30
                    link.Config.Id, link.Client.ReadBits [| address |])
        sw.Stop()
        match result with
        | Error m -> Error m
        | Ok(id, verify) ->
            log Trace (sprintf "··  WRITE BIT %s = %s  완료 %.0f ms" address (if value then "ON" else "OFF") sw.Elapsed.TotalMilliseconds)
            match verify.TryGetValue address with
            | true, rb ->
                lock cacheSync (fun () -> bitCache.[cacheKey id address] <- rb)
                valuesEvent.Trigger()
                Ok(Some rb)
            | _ ->
                valuesEvent.Trigger()
                Ok None

    member this.WriteBitVerified(address: string, value: bool) = this.WriteBitVerified(this.DefaultPlcId, address, value)

    /// WORD 쓰기 + 즉시 읽기 확인
    member this.WriteWordVerified(plcId: string, address: string, value: uint16) : Result<uint16, string> =
        let result =
            this.WithLink(
                plcId,
                fun link ->
                    link.Client.WriteWord(address, value)
                    link.Config.Id, link.Client.ReadWord address)
        match result with
        | Error m -> Error m
        | Ok(id, rb) ->
            lock cacheSync (fun () -> wordCache.[cacheKey id address] <- rb)
            valuesEvent.Trigger()
            Ok rb

    member this.WriteWordVerified(address: string, value: uint16) = this.WriteWordVerified(this.DefaultPlcId, address, value)

    /// UI 스레드를 막지 않도록 통신 작업은 백그라운드로 넘긴다.
    member this.RunAsync(work: unit -> 'T) : Task<'T> = Task.Run(fun () -> work ())

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
