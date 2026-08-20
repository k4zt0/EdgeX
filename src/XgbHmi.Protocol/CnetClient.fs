namespace XgbHmi.Protocol

open System
open System.Collections.Generic
open System.IO
open System.IO.Ports
open System.Text
open System.Threading

/// 직렬 회선 패리티. 화면/프로젝트 파일에서는 글자로 저장한다.
type CnetParity =
    | ParityNone
    | ParityOdd
    | ParityEven

    member this.Code =
        match this with
        | ParityNone -> "NONE"
        | ParityOdd -> "ODD"
        | ParityEven -> "EVEN"

[<RequireQualifiedAccess>]
module CnetParity =

    let all = [ ParityNone; ParityOdd; ParityEven ]

    let parse (s: string) =
        match (if isNull s then "" else s.Trim().ToUpperInvariant()) with
        | "ODD" -> ParityOdd
        | "EVEN" -> ParityEven
        | _ -> ParityNone

/// 직렬 연결 한 개의 설정. RS-232C 와 RS-485 는 회선만 다르고 프레임은 같다.
type CnetSettings =
    { PortName: string
      Baud: int
      DataBits: int
      Parity: CnetParity
      /// 1 또는 2
      StopBits: int
      /// Cnet 국번 (0~31). RS-485 는 이 번호로 여러 대를 구분한다.
      Station: int
      TimeoutMs: int
      /// RS-485(반이중) 인지. 같은 회선을 여러 국번이 나눠 쓰므로 프레임 사이를 조금 띄운다.
      HalfDuplex: bool }

/// XGT Cnet 전용 프로토콜(ASCII) 프레임 만들기/뜯기.
/// 하드웨어 없이도 시험할 수 있도록 순수 함수로 둔다.
[<RequireQualifiedAccess>]
module CnetFrame =

    let ENQ = 0x05uy
    let EOT = 0x04uy
    let ACK = 0x06uy
    let NAK = 0x15uy
    let ETX = 0x03uy

    /// 한 프레임에서 다룰 수 있는 블록 수 (XGT Cnet 사양)
    let maxBlocks = 16

    let private hex2 (value: int) = (value &&& 0xFF).ToString "X2"
    let private hex4 (value: int) = (value &&& 0xFFFF).ToString "X4"

    let hexDump (data: byte[]) =
        if isNull data then ""
        else data |> Array.map (fun b -> b.ToString "X2") |> String.concat " "

    /// 프레임 검사값. 머리(ENQ/ACK)부터 꼬리(EOT/ETX)까지 ASCII 값을 더한 하위 1바이트를 16진수 2자로 쓴다.
    let bcc (frame: byte[]) =
        let mutable sum = 0
        for b in frame do
            sum <- sum + int b
        hex2 sum

    /// 명령 글자가 대문자면 BCC 를 붙이고, 소문자면 붙이지 않는다. (XGT Cnet 규칙)
    let private letter (useBcc: bool) (command: char) =
        if useBcc then Char.ToUpperInvariant command else Char.ToLowerInvariant command

    let private build (useBcc: bool) (text: string) =
        let body = Array.append [| ENQ |] (Array.append (Encoding.ASCII.GetBytes text) [| EOT |])
        if useBcc then Array.append body (Encoding.ASCII.GetBytes(bcc body)) else body

    /// 개별 읽기(RSS). 한 프레임에 최대 16블록.
    let readFrame (useBcc: bool) (station: int) (names: string list) =
        if names.IsEmpty then raise (ArgumentException "읽을 주소가 없습니다.")
        if names.Length > maxBlocks then
            raise (ArgumentException(sprintf "한 프레임에서 최대 %d블록을 읽습니다." maxBlocks))
        let sb = StringBuilder()
        sb.Append(hex2 station).Append(letter useBcc 'R').Append("SS").Append(hex2 names.Length) |> ignore
        for n in names do
            sb.Append(hex2 n.Length).Append(n) |> ignore
        build useBcc (sb.ToString())

    /// 개별 쓰기(WSS). WORD 는 16진수 4자, BIT 는 2자로 값을 실어 보낸다.
    let private writeFrame (useBcc: bool) (station: int) (name: string) (data: string) =
        let sb = StringBuilder()
        sb
            .Append(hex2 station)
            .Append(letter useBcc 'W')
            .Append("SS")
            .Append(hex2 1)
            .Append(hex2 name.Length)
            .Append(name)
            .Append(data)
        |> ignore
        build useBcc (sb.ToString())

    let writeWordFrame (useBcc: bool) (station: int) (name: string) (value: uint16) =
        writeFrame useBcc station name (hex4 (int value))

    let writeBitFrame (useBcc: bool) (station: int) (name: string) (value: bool) =
        writeFrame useBcc station name (if value then "01" else "00")

    let private ascii (rx: byte[]) (offset: int) (length: int) =
        if offset + length > rx.Length then
            raise (IOException("Cnet 응답이 짧습니다: [" + hexDump rx + "]"))
        Encoding.ASCII.GetString(rx, offset, length)

    let private hexValue (rx: byte[]) (offset: int) (length: int) =
        let text = ascii rx offset length
        match Int32.TryParse(text, Globalization.NumberStyles.HexNumber, Globalization.CultureInfo.InvariantCulture) with
        | true, v -> v
        | _ -> raise (IOException("Cnet 응답의 16진수 자리가 잘못되었습니다 '" + text + "': [" + hexDump rx + "]"))

    /// 응답 프레임을 뜯는다. 읽기면 블록별 데이터 바이트, 쓰기면 빈 목록을 돌려준다.
    /// NAK 이면 PLC 가 준 오류 코드를 그대로 올린다.
    let parse (useBcc: bool) (station: int) (command: char) (rx: byte[]) : byte[] list =
        if isNull rx || rx.Length < 6 then
            raise (IOException("Cnet 응답이 없습니다: [" + hexDump rx + "]"))

        let head = rx.[0]
        if head <> ACK && head <> NAK then
            raise (IOException("Cnet 응답 머리가 ACK/NAK 가 아닙니다 0x" + head.ToString "X2" + ": [" + hexDump rx + "]"))

        let tail = Array.IndexOf(rx, ETX)
        if tail < 0 then
            raise (IOException("Cnet 응답에 ETX 가 없습니다: [" + hexDump rx + "]"))

        if useBcc && rx.Length >= tail + 3 then
            let expected = bcc rx.[0 .. tail]
            let actual = ascii rx (tail + 1) 2
            if not (String.Equals(expected, actual, StringComparison.OrdinalIgnoreCase)) then
                raise (IOException("Cnet 응답 BCC 불일치 (계산 " + expected + " / 수신 " + actual + "): [" + hexDump rx + "]"))

        let echoed = hexValue rx 1 2
        if echoed <> station then
            raise (IOException(sprintf "Cnet 응답 국번 불일치 (요청 %d / 응답 %d): [%s]" station echoed (hexDump rx)))

        let echoedCommand = (ascii rx 3 1).[0]
        if Char.ToUpperInvariant echoedCommand <> Char.ToUpperInvariant command then
            raise (IOException("Cnet 응답 명령 불일치 '" + string echoedCommand + "': [" + hexDump rx + "]"))

        if head = NAK then
            // NAK: 국번(2) + 명령(1) + 명령형식(2) + 오류코드(4)
            let code = if tail >= 10 then hexValue rx 6 4 else 0
            raise (
                XgtProtocolException(
                    sprintf "PLC Cnet 오류 0x%04X (국번 %d, %c%s): [%s]" code station command (ascii rx 4 2) (hexDump rx),
                    code
                )
            )

        if Char.ToUpperInvariant command <> 'R' then []
        else
            // ACK: 국번(2) + 명령(1) + 명령형식(2) + 블록수(2) + [데이터바이트수(2) + 데이터]*
            let blocks = hexValue rx 6 2
            if blocks < 1 then raise (IOException("Cnet 읽기 응답 블록 수가 0입니다: [" + hexDump rx + "]"))
            let result = ResizeArray<byte[]>()
            let mutable pos = 8
            for _ in 1..blocks do
                let count = hexValue rx pos 2
                pos <- pos + 2
                if count < 1 then raise (IOException("Cnet 읽기 응답 데이터 길이가 0입니다: [" + hexDump rx + "]"))
                let data = Array.init count (fun i -> byte (hexValue rx (pos + i * 2) 2))
                pos <- pos + count * 2
                result.Add data
            if pos > tail then
                raise (IOException("Cnet 읽기 응답 길이가 맞지 않습니다: [" + hexDump rx + "]"))
            List.ofSeq result

/// 직렬 회선 하나. RS-485 는 한 회선에 여러 국번이 붙으므로 요청을 잠금으로 한 줄로 세운다.
type ICnetTransport =
    inherit IDisposable
    /// 요청을 보내고 응답 한 프레임(ETX + BCC 까지)을 받는다.
    abstract Exchange: request: byte[] * expectBcc: bool -> byte[]
    /// 화면에 보여 줄 회선 이름 (예: "COM3 9600-8-N-1")
    abstract Description: string

/// 실제 직렬 포트. 같은 포트를 여러 국번이 나눠 쓰도록 등록소에서 공유한다.
type internal SerialBus(settings: CnetSettings) =

    let sync = obj ()
    let parity =
        match settings.Parity with
        | ParityOdd -> Parity.Odd
        | ParityEven -> Parity.Even
        | ParityNone -> Parity.None
    let stopBits = if settings.StopBits >= 2 then StopBits.Two else StopBits.One
    let port = new SerialPort(settings.PortName, settings.Baud, parity, settings.DataBits, stopBits)
    let mutable leases = 0

    let description =
        sprintf
            "%s %d-%d-%s-%d"
            settings.PortName
            settings.Baud
            settings.DataBits
            (match settings.Parity with
             | ParityNone -> "N"
             | ParityOdd -> "O"
             | ParityEven -> "E")
            settings.StopBits

    /// 같은 포트를 서로 다른 통신 속도로 열 수는 없다. 열려 있는 설정과 다르면 알려 준다.
    member _.Matches(other: CnetSettings) =
        other.Baud = settings.Baud
        && other.DataBits = settings.DataBits
        && other.Parity = settings.Parity
        && other.StopBits = settings.StopBits

    member _.Description = description
    member _.Leases = leases

    member _.Open(timeoutMs: int) =
        lock sync (fun () ->
            if not port.IsOpen then
                port.ReadTimeout <- timeoutMs
                port.WriteTimeout <- timeoutMs
                port.Handshake <- Handshake.None
                port.DtrEnable <- true
                port.RtsEnable <- true
                port.Open()
            leases <- leases + 1)

    member _.Release() =
        lock sync (fun () ->
            leases <- max 0 (leases - 1)
            if leases = 0 && port.IsOpen then
                (try port.Close() with _ -> ()))
        leases

    member _.Exchange(request: byte[], expectBcc: bool) =
        lock sync (fun () ->
            if not port.IsOpen then raise (InvalidOperationException("직렬 포트가 닫혔습니다: " + settings.PortName))
            (try port.DiscardInBuffer() with _ -> ())
            port.Write(request, 0, request.Length)

            let buffer = ResizeArray<byte>()
            let mutable finished = false
            while not finished do
                let b = byte (port.ReadByte())
                // 응답 앞에 남아 있는 잡음은 버린다. 머리(ACK/NAK)부터 담는다.
                if buffer.Count = 0 then
                    if b = CnetFrame.ACK || b = CnetFrame.NAK then buffer.Add b
                else
                    buffer.Add b
                    if b = CnetFrame.ETX then
                        if expectBcc then
                            buffer.Add(byte (port.ReadByte()))
                            buffer.Add(byte (port.ReadByte()))
                        finished <- true
                if buffer.Count > 4096 then
                    raise (IOException("Cnet 응답이 너무 깁니다: [" + CnetFrame.hexDump (buffer.ToArray()) + "]"))

            // 반이중(RS-485) 회선은 다음 국번이 바로 말하지 않도록 조금 띄운다.
            if settings.HalfDuplex then Thread.Sleep 3
            buffer.ToArray())

    member _.Dispose() =
        (try if port.IsOpen then port.Close() with _ -> ())
        (try port.Dispose() with _ -> ())

/// 열려 있는 직렬 회선 등록소. RS-485 다중 접속(한 포트 + 여러 국번)을 여기서 묶는다.
[<RequireQualifiedAccess>]
module SerialBusRegistry =

    let private sync = obj ()
    let private buses = Dictionary<string, SerialBus>(StringComparer.OrdinalIgnoreCase)

    /// 지금 컴퓨터에 있는 직렬 포트 목록. (USB-직렬 변환기를 꽂으면 여기에 나온다)
    let availablePorts () =
        try SerialPort.GetPortNames() |> Array.sort with _ -> [||]

    /// 회선을 빌린다. 같은 포트를 이미 다른 국번이 쓰고 있으면 그 회선을 함께 쓴다.
    let acquire (settings: CnetSettings) : ICnetTransport =
        let bus =
            lock sync (fun () ->
                match buses.TryGetValue settings.PortName with
                | true, existing ->
                    if not (existing.Matches settings) then
                        raise (
                            InvalidOperationException(
                                settings.PortName
                                + " 는 이미 "
                                + existing.Description
                                + " 로 열려 있습니다. 같은 회선에 붙는 PLC 는 통신 속도·패리티·정지 비트를 같게 맞추십시오."
                            )
                        )
                    existing
                | _ ->
                    let created = new SerialBus(settings)
                    buses.[settings.PortName] <- created
                    created)
        bus.Open settings.TimeoutMs
        let mutable released = false
        { new ICnetTransport with
            member _.Exchange(request, expectBcc) = bus.Exchange(request, expectBcc)
            member _.Description = bus.Description
            member _.Dispose() =
                if not released then
                    released <- true
                    if bus.Release() = 0 then
                        lock sync (fun () ->
                            if bus.Leases = 0 then
                                buses.Remove settings.PortName |> ignore
                                bus.Dispose()) }

/// LS ELECTRIC XGB / XGT Cnet(RS-232C · RS-485) 전용 클라이언트.
/// 프레임만 다르고 주소 해석과 M비트 쓰기 규칙은 이더넷(FEnet) 과 똑같다.
type CnetClient(settings: CnetSettings, transportFactory: CnetSettings -> ICnetTransport) =

    let traceEvent = Event<XgtTrace>()
    let mutable traceEnabled = false
    let mutable frameCount = 0L
    let mutable errorCount = 0L
    let mutable transport: ICnetTransport option = None
    /// 대문자 명령(BCC 사용) 과 소문자 명령(BCC 없음) 중 실제로 통하는 쪽
    let mutable useBcc = true
    let mutable negotiationLog = ""
    let mutable profileName = "미확정"

    let trace kind summary hexText elapsed =
        traceEvent.Trigger { Kind = kind; Summary = summary; Hex = hexText; ElapsedMs = elapsed }

    let station = max 0 (min 31 settings.Station)

    let lineName () =
        match transport with
        | Some t -> t.Description
        | None -> settings.PortName

    let exchange (summary: string) (command: char) (request: byte[]) =
        match transport with
        | None -> raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
        | Some t ->
            let sw = Diagnostics.Stopwatch.StartNew()
            if traceEnabled then
                trace Tx (sprintf "%s  (%d bytes)" summary request.Length) (CnetFrame.hexDump request) 0.0
            frameCount <- frameCount + 1L
            let response =
                try t.Exchange(request, useBcc)
                with ex ->
                    errorCount <- errorCount + 1L
                    raise ex
            sw.Stop()
            let blocks =
                try CnetFrame.parse useBcc station command response
                with ex ->
                    errorCount <- errorCount + 1L
                    raise ex
            if traceEnabled then
                trace
                    Rx
                    (sprintf "%s  ->  %s 블록 %d개 (%d bytes, %.1f ms)" summary (if response.[0] = CnetFrame.ACK then "ACK" else "NAK") blocks.Length response.Length sw.Elapsed.TotalMilliseconds)
                    (CnetFrame.hexDump response)
                    sw.Elapsed.TotalMilliseconds
            blocks

    /// 블록 하나의 데이터 바이트를 WORD 값으로 읽는다. Cnet 은 16진수 글자를 상위 바이트부터 보낸다.
    let toWord (data: byte[]) =
        if isNull data || data.Length = 0 then raise (IOException "Cnet 응답 블록이 비었습니다.")
        elif data.Length = 1 then uint16 data.[0]
        else (uint16 data.[0] <<< 8) ||| uint16 data.[1]

    /// 한 프레임에서 최대 16 WORD 를 읽는다. (이더넷과 같은 제한)
    let readAreaWords (area: char) (words: IList<int>) =
        let result = Dictionary<int, uint16>()
        if isNull (box words) || words.Count = 0 then result
        else
            if words.Count > CnetFrame.maxBlocks then
                raise (ArgumentException(sprintf "한 프레임에서 최대 %d WORD를 읽습니다." CnetFrame.maxBlocks))
            let area = Char.ToUpperInvariant area
            if area <> 'P' && area <> 'M' && area <> 'D' then
                raise (ArgumentException("지원하지 않는 WORD 영역: " + string area))

            let names = [ for i in 0 .. words.Count - 1 -> Address.toXgtWord area words.[i] ]
            let summary = sprintf "READ WORD %s x%d (국번 %d)" (String.Join(",", names)) words.Count station
            let blocks = exchange summary 'R' (CnetFrame.readFrame useBcc station names)
            let count = min blocks.Length words.Count
            for i in 0 .. count - 1 do
                result.[words.[i]] <- toWord blocks.[i]
            result

    let writeAreaWord (area: char) (word: int) (value: uint16) =
        let area = Char.ToUpperInvariant area
        if area <> 'M' && area <> 'D' && area <> 'P' then
            raise (ArgumentException("지원하지 않는 WORD 쓰기 영역: " + string area))
        let name = Address.toXgtWord area word
        exchange
            (sprintf "WRITE WORD %s = %d (0x%04X) (국번 %d)" name value value station)
            'W'
            (CnetFrame.writeWordFrame useBcc station name value)
        |> ignore

    let writeMBitByWord (address: string) (value: bool) =
        MBit.writeByWord
            (fun word ->
                let map = readAreaWords 'M' [| word |]
                match map.TryGetValue word with
                | true, v -> Some v
                | _ -> None)
            (fun word v -> writeAreaWord 'M' word v)
            (fun text -> trace Note text "" 0.0)
            address
            value

    /// 이더넷과 마찬가지로 %MW0 을 한 번 읽어 회선이 실제로 통하는지 본다.
    let probeKnownRead () =
        let blocks = exchange "PROBE READ WORD %MW0 x1" 'R' (CnetFrame.readFrame useBcc station [ "%MW0" ])
        if blocks.IsEmpty then raise (IOException "%MW0 시험 읽기 응답 블록이 없습니다.")

    new(settings: CnetSettings) = new CnetClient(settings, SerialBusRegistry.acquire)

    member _.Trace = traceEvent.Publish

    member _.TraceEnabled
        with get () = traceEnabled
        and set v = traceEnabled <- v

    member _.FrameCount = frameCount
    member _.ErrorCount = errorCount
    member _.ProfileName = profileName
    member _.NegotiationLog = negotiationLog
    member _.Station = station
    member _.Connected = transport.IsSome

    member _.Dispose() =
        match transport with
        | Some t ->
            transport <- None
            (try t.Dispose() with _ -> ())
        | None -> ()

    /// 회선을 열고, BCC 를 붙이는 프레임과 붙이지 않는 프레임을 차례로 시험한다.
    /// (Cnet 모듈 설정에 따라 둘 중 한쪽만 받는 경우가 있다)
    member this.Connect() =
        this.Dispose()
        let log = StringBuilder()
        let opened = transportFactory settings
        transport <- Some opened

        let candidates = [ true, "BCC 사용"; false, "BCC 없음" ]
        let mutable last: exn = null
        let mutable ok = false
        for (bcc, label) in candidates do
            if not ok then
                let sw = Diagnostics.Stopwatch.StartNew()
                try
                    useBcc <- bcc
                    probeKnownRead ()
                    sw.Stop()
                    profileName <- sprintf "Cnet %s / 국번 %d / %s" (lineName ()) station label
                    let line = sprintf "OK   %s  (%.0f ms)" profileName sw.Elapsed.TotalMilliseconds
                    log.AppendLine line |> ignore
                    trace Note line "" sw.Elapsed.TotalMilliseconds
                    ok <- true
                with ex ->
                    sw.Stop()
                    last <- ex
                    let line = sprintf "FAIL Cnet %s / 국번 %d / %s  (%.0f ms) : %s" (lineName ()) station label sw.Elapsed.TotalMilliseconds ex.Message
                    log.AppendLine line |> ignore
                    trace Note line "" sw.Elapsed.TotalMilliseconds

        negotiationLog <- log.ToString()
        if not ok then
            this.Dispose()
            profileName <- "미확정"
            raise (
                IOException(
                    "Cnet 자동 판별 실패 ("
                    + settings.PortName
                    + ", 국번 "
                    + string station
                    + "). 통신 속도·패리티·국번을 확인하십시오."
                    + (if isNull last then "" else " 마지막 오류: " + last.Message)
                )
            )

    member _.ReadBits(addresses: IList<string>) : Dictionary<string, bool> =
        let result = Dictionary<string, bool>(StringComparer.OrdinalIgnoreCase)
        if isNull (box addresses) || addresses.Count = 0 then result
        else
            let areaWords = Dictionary<char, ResizeArray<int>>()
            for a in addresses do
                let b = Address.parseBit a
                if not (areaWords.ContainsKey b.Area) then areaWords.[b.Area] <- ResizeArray<int>()
                if not (areaWords.[b.Area].Contains b.Word) then areaWords.[b.Area].Add b.Word

            let valuesByArea = Dictionary<char, Dictionary<int, uint16>>()
            for kv in areaWords do
                valuesByArea.[kv.Key] <- readAreaWords kv.Key kv.Value

            for a in addresses do
                let b = Address.parseBit a
                match valuesByArea.TryGetValue b.Area with
                | true, map ->
                    match map.TryGetValue b.Word with
                    | true, w -> result.[a] <- (w &&& uint16 (1 <<< b.Bit)) <> 0us
                    | _ -> ()
                | _ -> ()
            result

    member _.ReadWord(address: string) : uint16 =
        let word = Address.parseDWord "읽기" address
        let vals = readAreaWords 'D' [| word |]
        match vals.TryGetValue word with
        | true, v -> v
        | _ -> raise (IOException(address + " 읽기 응답에 데이터가 없습니다."))

    member _.WriteBit(address: string, value: bool) =
        let b = Address.parseBit address
        // 이더넷과 같은 규칙: M 영역은 WORD 읽고-고치고-쓰기, P 영역은 비트 직접 쓰기.
        if b.Area = 'M' then writeMBitByWord address value
        else
            let name = Address.toXgtBit address
            exchange
                (sprintf "WRITE BIT %s = %s (국번 %d)" name (if value then "ON" else "OFF") station)
                'W'
                (CnetFrame.writeBitFrame useBcc station name value)
            |> ignore

    member _.WriteWord(address: string, value: uint16) =
        writeAreaWord 'D' (Address.parseDWord "쓰기" address) value

    interface IDisposable with
        member this.Dispose() = this.Dispose()

    interface IPlcLink with
        member this.Connect() = this.Connect()
        member this.Connected = this.Connected
        member this.ProfileName = this.ProfileName
        member this.NegotiationLog = this.NegotiationLog
        member this.FrameCount = this.FrameCount
        member this.ErrorCount = this.ErrorCount
        member this.TraceEnabled
            with get () = this.TraceEnabled
            and set v = this.TraceEnabled <- v
        member this.Trace = this.Trace
        member this.ReadBits addresses = this.ReadBits addresses
        member this.ReadWord address = this.ReadWord address
        member this.WriteBit(address, value) = this.WriteBit(address, value)
        member this.WriteWord(address, value) = this.WriteWord(address, value)
