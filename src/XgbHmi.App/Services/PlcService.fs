namespace XgbHmi.App.Services

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
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

/// PLC 연결 / 주기 폴링 / 쓰기를 담당한다.
/// XgtClient 는 스레드 안전하지 않으므로 모든 접근을 하나의 락으로 직렬화한다. (원본 _sync 와 동일)
type PlcService() =

    let sync = obj ()
    let cacheSync = obj ()

    let bitCache = Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase)
    let wordCache = Dictionary<string, uint16>(StringComparer.OrdinalIgnoreCase)

    let logEvent = Event<LogLevel * string>()
    let stateEvent = Event<ConnState * string>()
    let valuesEvent = Event<unit>()

    let mutable client: XgtClient option = None
    let mutable worker: Thread = null
    let mutable running = false
    let mutable cycleMs = 300
    let mutable scanProvider: unit -> string list * string list = fun () -> [], []
    let mutable traceEnabled = false
    let mutable logChanges = true
    let mutable lastCycleMs = 0.0
    let mutable cycleCount = 0L

    let log level (message: string) = logEvent.Trigger(level, message)

    /// XgtClient 의 추적을 화면 로그로 넘긴다.
    let attachTrace (c: XgtClient) =
        c.TraceEnabled <- traceEnabled
        c.Trace.Add(fun t ->
            let tag =
                match t.Kind with
                | Tx -> "TX  "
                | Rx -> "RX  "
                | Note -> "··  "
            let line =
                if String.IsNullOrEmpty t.Hex then tag + t.Summary
                else tag + t.Summary + "\n      " + t.Hex
            logEvent.Trigger(Trace, line))

    let setCacheBits (values: Dictionary<string, bool>) =
        let changes = ResizeArray<string>()
        lock cacheSync (fun () ->
            for kv in values do
                match bitCache.TryGetValue kv.Key with
                | true, previous when previous <> kv.Value ->
                    changes.Add(sprintf "%s : %s -> %s" kv.Key (if previous then "ON" else "OFF") (if kv.Value then "ON" else "OFF"))
                | _ -> ()
                bitCache.[kv.Key] <- kv.Value)
        if logChanges then
            for c in changes do
                logEvent.Trigger(Info, "CHANGE " + c)

    let setCacheWords (values: Dictionary<string, uint16>) =
        let changes = ResizeArray<string>()
        lock cacheSync (fun () ->
            for kv in values do
                match wordCache.TryGetValue kv.Key with
                | true, previous when previous <> kv.Value ->
                    changes.Add(sprintf "%s : %d -> %d (0x%04X, signed %d)" kv.Key previous kv.Value kv.Value (int16 kv.Value))
                | _ -> ()
                wordCache.[kv.Key] <- kv.Value)
        if logChanges then
            for c in changes do
                logEvent.Trigger(Info, "CHANGE " + c)

    member _.Log = logEvent.Publish
    member _.StateChanged = stateEvent.Publish
    member _.ValuesChanged = valuesEvent.Publish

    member _.IsRunning = running

    member _.ProfileName =
        match client with
        | Some c -> c.ProfileName
        | None -> ""

    member _.CycleMs
        with get () = cycleMs
        and set v = cycleMs <- max 100 (min 5000 v)

    /// TX/RX 프레임 원문까지 출력 창에 남길지
    member _.TraceEnabled
        with get () = traceEnabled
        and set v =
            traceEnabled <- v
            match client with
            | Some c -> c.TraceEnabled <- v
            | None -> ()

    /// 값이 바뀔 때마다 출력 창에 남길지
    member _.LogChanges
        with get () = logChanges
        and set v = logChanges <- v

    /// 마지막 스캔에 걸린 시간(ms)
    member _.LastCycleMs = lastCycleMs

    /// 지금까지의 스캔 횟수 / 프레임 수 / 오류 수
    member _.CycleCount = cycleCount

    member _.FrameCount =
        match client with
        | Some c -> c.FrameCount
        | None -> 0L

    member _.ErrorCount =
        match client with
        | Some c -> c.ErrorCount
        | None -> 0L

    /// 폴링할 주소 목록을 돌려주는 함수 (비트 목록, WORD 목록)
    member _.SetScanProvider(provider: unit -> string list * string list) = scanProvider <- provider

    member _.TryBit(address: string) =
        lock cacheSync (fun () ->
            match bitCache.TryGetValue address with
            | true, v -> Some v
            | _ -> None)

    member _.TryWord(address: string) =
        lock cacheSync (fun () ->
            match wordCache.TryGetValue address with
            | true, v -> Some v
            | _ -> None)

    member private this.WorkerLoop() =
        while running do
            let sw = Diagnostics.Stopwatch.StartNew()
            try
                let bits, words = scanProvider ()

                let bvals = Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase)
                let bitArray = List.toArray bits
                let mutable i = 0
                let mutable aborted = false
                while not aborted && i < bitArray.Length do
                    let chunk = bitArray.[i .. min (i + 15) (bitArray.Length - 1)]
                    let part =
                        lock sync (fun () ->
                            match client with
                            | None -> null
                            | Some c -> c.ReadBits chunk)
                    if isNull part then aborted <- true
                    else
                        for kv in part do
                            bvals.[kv.Key] <- kv.Value
                        i <- i + 16

                if not aborted then
                    let wvals = Dictionary<string, uint16>(StringComparer.OrdinalIgnoreCase)
                    for d in words do
                        if not aborted then
                            let v =
                                lock sync (fun () ->
                                    match client with
                                    | None -> None
                                    | Some c -> Some(c.ReadWord d))
                            match v with
                            | None -> aborted <- true
                            | Some value -> wvals.[d] <- value

                    if not aborted then
                        setCacheBits bvals
                        setCacheWords wvals
                        sw.Stop()
                        lastCycleMs <- sw.Elapsed.TotalMilliseconds
                        cycleCount <- cycleCount + 1L
                        if running then
                            valuesEvent.Trigger()
                            stateEvent.Trigger(
                                Online,
                                sprintf
                                    "%s   비트 %d · WORD %d   스캔 %.0f ms"
                                    (DateTime.Now.ToString "HH:mm:ss.fff")
                                    bitArray.Length
                                    (List.length words)
                                    lastCycleMs)
                            if traceEnabled then
                                logEvent.Trigger(
                                    Trace,
                                    sprintf
                                        "SCAN #%d  비트 %d개(프레임 %d) · WORD %d개  →  %.1f ms"
                                        cycleCount
                                        bitArray.Length
                                        ((bitArray.Length + 15) / 16)
                                        (List.length words)
                                        lastCycleMs)
            with ex ->
                sw.Stop()
                stateEvent.Trigger(Faulted, ex.Message)
                log Failure (sprintf "READ ERROR (%.0f ms, %s): %s" sw.Elapsed.TotalMilliseconds (ex.GetType().Name) ex.Message)
                Thread.Sleep 700

            Thread.Sleep cycleMs

    member this.Connect(ip: string, port: int, cycle: int) : Result<string, string> =
        try
            stateEvent.Trigger(Connecting, "")
            let c = new XgtClient(ip, port, 1800)
            attachTrace c
            log Info (sprintf "CONNECT %s:%d (타임아웃 1800 ms, 주기 %d ms) — 헤더 조합 8종 자동 시험" ip port cycle)
            c.Connect()
            client <- Some c

            // 원본과 동일하게 P00000 을 한 번 읽어 캐시를 채운다.
            let probe = lock sync (fun () -> c.ReadBits [| "P00000" |])
            setCacheBits probe

            cycleMs <- max 100 (min 5000 cycle)
            running <- true
            worker <- Thread(ThreadStart(this.WorkerLoop), IsBackground = true)
            worker.Start()

            stateEvent.Trigger(Online, DateTime.Now.ToString "HH:mm:ss.fff")
            log Success (sprintf "CONNECTED %s:%d  헤더 프로필=%s  프레임 %d개 교환" ip port c.ProfileName c.FrameCount)
            let negotiation = c.NegotiationLog.TrimEnd()
            if not (String.IsNullOrWhiteSpace negotiation) then log Info negotiation
            Ok c.ProfileName
        with ex ->
            let diag =
                match client with
                | Some c -> c.NegotiationLog
                | None -> ""
            match client with
            | Some c -> (c :> IDisposable).Dispose()
            | None -> ()
            client <- None
            running <- false
            stateEvent.Trigger(Faulted, ex.Message)
            log Failure ("CONNECT/XGT ERROR: " + ex.Message)
            if not (String.IsNullOrWhiteSpace diag) then log Info (diag.TrimEnd())
            Error ex.Message

    member _.Disconnect() =
        (match client with
         | Some c when running ->
             log Info (sprintf "DISCONNECT 요청 — 스캔 %d회, 프레임 %d개, 오류 %d개" cycleCount c.FrameCount c.ErrorCount)
         | _ -> ())
        running <- false
        lock sync (fun () ->
            match client with
            | Some c -> (c :> IDisposable).Dispose()
            | None -> ()
            client <- None)
        stateEvent.Trigger(Disconnected, "")
        log Info "Disconnected"

    /// 토글용: 화면 캐시가 아니라 PLC의 실제 비트를 즉시 읽는다. (v4 토글 수정)
    member _.ReadBitNow(address: string) : Result<bool, string> =
        try
            let fresh =
                lock sync (fun () ->
                    match client with
                    | None -> raise (InvalidOperationException "PLC 연결이 없습니다.")
                    | Some c -> c.ReadBits [| address |])
            match fresh.TryGetValue address with
            | true, v ->
                lock cacheSync (fun () -> bitCache.[address] <- v)
                Ok v
            | _ -> Error(address + " 현재값 읽기 응답이 없습니다.")
        with ex -> Error ex.Message

    /// 비트 쓰기 + 30ms 후 읽기 확인 (원본 WriteBitForItem 과 동일한 절차)
    member _.WriteBitVerified(address: string, value: bool) : Result<bool option, string> =
        try
            let sw = Diagnostics.Stopwatch.StartNew()
            let verify =
                lock sync (fun () ->
                    match client with
                    | None -> raise (InvalidOperationException "PLC 연결이 없습니다.")
                    | Some c ->
                        c.WriteBit(address, value)
                        Thread.Sleep 30
                        c.ReadBits [| address |])
            sw.Stop()
            log Trace (sprintf "··  WRITE BIT %s = %s  완료 %.0f ms" address (if value then "ON" else "OFF") sw.Elapsed.TotalMilliseconds)
            match verify.TryGetValue address with
            | true, rb ->
                lock cacheSync (fun () -> bitCache.[address] <- rb)
                valuesEvent.Trigger()
                Ok(Some rb)
            | _ ->
                valuesEvent.Trigger()
                Ok None
        with ex -> Error ex.Message

    /// WORD 쓰기 + 즉시 읽기 확인
    member _.WriteWordVerified(address: string, value: uint16) : Result<uint16, string> =
        try
            let rb =
                lock sync (fun () ->
                    match client with
                    | None -> raise (InvalidOperationException "PLC 연결이 없습니다.")
                    | Some c ->
                        c.WriteWord(address, value)
                        c.ReadWord address)
            lock cacheSync (fun () -> wordCache.[address] <- rb)
            valuesEvent.Trigger()
            Ok rb
        with ex -> Error ex.Message

    /// UI 스레드를 막지 않도록 통신 작업은 백그라운드로 넘긴다.
    member this.RunAsync(work: unit -> 'T) : Task<'T> = Task.Run(fun () -> work ())

    interface IDisposable with
        member this.Dispose() = this.Disconnect()
