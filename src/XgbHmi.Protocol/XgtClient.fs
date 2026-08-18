namespace XgbHmi.Protocol

open System
open System.Collections.Generic
open System.IO
open System.Net.Sockets
open System.Text
open System.Threading

/// PLC가 되돌려 준 XGT 오류 코드를 그대로 담는다.
exception XgtProtocolException of message: string * code: int

/// 통신 추적 한 줄. 화면 출력 창에서 실제 오간 내용을 그대로 보여 주기 위한 것.
type XgtTraceKind =
    | Tx
    | Rx
    | Note

type XgtTrace =
    { Kind: XgtTraceKind
      /// 사람이 읽는 요약 (예: "READ WORD %MW100 x1")
      Summary: string
      /// 실제 바이트 (추적을 켰을 때만 채운다)
      Hex: string
      ElapsedMs: float }

/// XGT FEnet 헤더 조합. 펌웨어/모듈 설정에 따라 다르므로 연결할 때 자동으로 시험한다.
type internal HeaderProfile =
    { Company: string
      CpuInfo: byte
      Position: byte
      UseBcc: bool
      Name: string }

/// LS ELECTRIC XGB(MK) / XGT FEnet 전용 TCP 클라이언트.
/// WinForms v6 의 XgtClient 와 바이트 단위로 동일한 프레임을 만든다.
/// .NET 소켓만 쓰므로 Windows / macOS / Linux 에서 동일하게 동작한다.
type XgtClient(ip: string, port: int, timeoutMs: int) =

    let mutable tcp: TcpClient = null
    let mutable stream: NetworkStream = null
    let mutable invokeId: uint16 = 1us
    let mutable profile: HeaderProfile option = None
    let mutable negotiationLog = ""
    let traceEvent = Event<XgtTrace>()
    let mutable traceEnabled = false
    let mutable frameCount = 0L
    let mutable errorCount = 0L

    let trace kind summary hexText elapsed =
        traceEvent.Trigger { Kind = kind; Summary = summary; Hex = hexText; ElapsedMs = elapsed }

    static let candidates =
        [| { Company = "LSIS-XGT"; CpuInfo = 0xB0uy; Position = 0x01uy; UseBcc = true; Name = "XGB(MK) / Slot1 / BCC" }
           { Company = "LSIS-XGT"; CpuInfo = 0xB0uy; Position = 0x00uy; UseBcc = true; Name = "XGB(MK) / Slot0 / BCC" }
           { Company = "LSIS-XGT"; CpuInfo = 0xB0uy; Position = 0x01uy; UseBcc = false; Name = "XGB(MK) / Slot1 / BCC=00" }
           { Company = "LSIS-XGT"; CpuInfo = 0xB0uy; Position = 0x00uy; UseBcc = false; Name = "XGB(MK) / Slot0 / BCC=00" }
           { Company = "LGIS-GLOFA"; CpuInfo = 0xB0uy; Position = 0x01uy; UseBcc = true; Name = "XGB(MK) / GLOFA / Slot1 / BCC" }
           { Company = "LGIS-GLOFA"; CpuInfo = 0xB0uy; Position = 0x00uy; UseBcc = true; Name = "XGB(MK) / GLOFA / Slot0 / BCC" }
           { Company = "LGIS-GLOFA"; CpuInfo = 0xB0uy; Position = 0x01uy; UseBcc = false; Name = "XGB(MK) / GLOFA / Slot1 / BCC=00" }
           { Company = "LGIS-GLOFA"; CpuInfo = 0xB0uy; Position = 0x00uy; UseBcc = false; Name = "XGB(MK) / GLOFA / Slot0 / BCC=00" } |]

    static let uint16LE (value: int) =
        [| byte (value &&& 0xFF); byte ((value >>> 8) &&& 0xFF) |]

    static let readUInt16LE (data: byte[]) (offset: int) =
        int data.[offset] ||| (int data.[offset + 1] <<< 8)

    static let hex (data: byte[]) =
        if isNull data then ""
        else data |> Array.map (fun b -> b.ToString "X2") |> String.concat " "

    let disposeSocket () =
        (try if not (isNull stream) then stream.Close() with _ -> ())
        (try if not (isNull tcp) then tcp.Close() with _ -> ())
        stream <- null
        tcp <- null

    let connectSocket () =
        disposeSocket ()
        let client = new TcpClient()
        tcp <- client
        let ar = client.BeginConnect(ip, port, null, null)
        if not (ar.AsyncWaitHandle.WaitOne timeoutMs) then
            (try client.Close() with _ -> ())
            raise (TimeoutException "PLC TCP 연결 시간 초과")
        client.EndConnect ar
        client.NoDelay <- true
        stream <- client.GetStream()
        stream.ReadTimeout <- timeoutMs
        stream.WriteTimeout <- timeoutMs
        invokeId <- 1us

    let buildHeader (bodyLength: int) =
        match profile with
        | None -> raise (InvalidOperationException "XGT 헤더 프로필이 선택되지 않았습니다.")
        | Some p ->
            let header = Array.zeroCreate<byte> 20
            let company =
                if p.Company = "LGIS-GLOFA" then Encoding.ASCII.GetBytes "LGIS-GLOFA"
                else Encoding.ASCII.GetBytes "LSIS-XGT\000\000"
            Buffer.BlockCopy(company, 0, header, 0, 10)
            header.[10] <- 0x00uy // PLC Info: Client -> Server don't care
            header.[11] <- 0x00uy
            header.[12] <- p.CpuInfo
            header.[13] <- 0x33uy // Client -> Server
            header.[14] <- byte (invokeId &&& 0xFFus)
            header.[15] <- byte ((invokeId >>> 8) &&& 0xFFus)
            header.[16] <- byte (bodyLength &&& 0xFF)
            header.[17] <- byte ((bodyLength >>> 8) &&& 0xFF)
            header.[18] <- p.Position
            if p.UseBcc then
                let mutable sum = 0
                for i in 0..18 do
                    sum <- sum + int header.[i]
                header.[19] <- byte (sum &&& 0xFF)
            else
                header.[19] <- 0x00uy
            invokeId <- invokeId + 1us
            header

    let readExact (count: int) =
        let data = Array.zeroCreate<byte> count
        let mutable offset = 0
        while offset < count do
            let n = stream.Read(data, offset, count - offset)
            if n <= 0 then raise (IOException "PLC가 TCP 연결을 종료했습니다.")
            offset <- offset + n
        data

    let exchangeWith (summary: string) (bodyBytes: byte[]) =
        if isNull stream then raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
        let sw = Diagnostics.Stopwatch.StartNew()
        let header = buildHeader bodyBytes.Length
        let tx = Array.append header bodyBytes
        stream.Write(tx, 0, tx.Length)
        stream.Flush()
        frameCount <- frameCount + 1L
        if traceEnabled then
            trace Tx (sprintf "%s  (%d bytes)" summary tx.Length) (hex tx) 0.0

        let rh = readExact 20
        let company8 = Encoding.ASCII.GetString(rh, 0, 8)
        let company10 = Encoding.ASCII.GetString(rh, 0, 10).TrimEnd([| '\000'; ' ' |])
        if not (company8 = "LSIS-XGT" || company10 = "LGIS-GLOFA") then
            errorCount <- errorCount + 1L
            raise (IOException("XGT 헤더 ID 불일치. TX=[" + hex tx + "] RXH=[" + hex rh + "]"))
        if rh.[13] <> 0x11uy then
            errorCount <- errorCount + 1L
            raise (IOException("XGT 응답 방향 오류 0x" + rh.[13].ToString "X2" + ". TX=[" + hex tx + "] RXH=[" + hex rh + "]"))

        let responseLength = readUInt16LE rh 16
        if responseLength <= 0 || responseLength > 4096 then
            errorCount <- errorCount + 1L
            raise (IOException("XGT 응답 Length=" + string responseLength + ". TX=[" + hex tx + "] RXH=[" + hex rh + "]"))

        let rb = readExact responseLength
        sw.Stop()
        if traceEnabled then
            let status = readUInt16LE rb 6
            trace
                Rx
                (sprintf "%s  ->  cmd=0x%04X status=0x%04X (%d bytes, %.1f ms)" summary (readUInt16LE rb 0) status (20 + rb.Length) sw.Elapsed.TotalMilliseconds)
                (hex (Array.append rh rb))
                sw.Elapsed.TotalMilliseconds
        rb

    /// XGB FEnet 구버전 매뉴얼에도 있는 가장 기본적인 프레임 그대로 사용:
    /// Read Individual / WORD / 1 block / %MW0
    let probeKnownRead () =
        let body =
            [| 0x54uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy
               0x04uy; 0x00uy; 0x25uy; 0x4Duy; 0x57uy; 0x30uy |]
        let rb = exchangeWith "PROBE READ WORD %MW0 x1" body
        if rb.Length < 10 then raise (IOException("%MW0 시험 읽기 응답 데이터 부족: " + string rb.Length))
        let cmd = readUInt16LE rb 0
        if cmd <> 0x0055 then raise (IOException("%MW0 시험 읽기 응답 명령 오류: 0x" + cmd.ToString "X4"))
        let error = readUInt16LE rb 6
        if error <> 0 then
            let detail = if rb.Length >= 10 then readUInt16LE rb 8 else error
            raise (XgtProtocolException("%MW0 시험 읽기 오류 0x" + detail.ToString "X4", detail))
        let blocks = readUInt16LE rb 8
        if blocks < 1 then raise (IOException "%MW0 시험 읽기 블록 수가 0입니다.")

    /// 한 프레임에서 최대 16 WORD 를 연속 읽는다.
    let readAreaWords (area: char) (words: IList<int>) =
        let result = Dictionary<int, uint16>()
        if isNull (box words) || words.Count = 0 then result
        else
            if words.Count > 16 then raise (ArgumentException "한 프레임에서 최대 16 WORD를 읽습니다.")
            let area = Char.ToUpperInvariant area
            if area <> 'P' && area <> 'M' && area <> 'D' then
                raise (ArgumentException("지원하지 않는 WORD 영역: " + string area))

            let body = new MemoryStream()
            body.WriteByte 0x54uy
            body.WriteByte 0x00uy
            body.WriteByte 0x02uy
            body.WriteByte 0x00uy
            body.WriteByte 0x00uy
            body.WriteByte 0x00uy
            let bc = uint16LE words.Count
            body.Write(bc, 0, bc.Length)

            for i in 0 .. words.Count - 1 do
                let name = Encoding.ASCII.GetBytes(Address.toXgtWord area words.[i])
                let len = uint16LE name.Length
                body.Write(len, 0, len.Length)
                body.Write(name, 0, name.Length)

            let names = [ for i in 0 .. words.Count - 1 -> Address.toXgtWord area words.[i] ]
            let summary = sprintf "READ WORD %s x%d" (String.Join(",", names)) words.Count
            let rb = exchangeWith summary (body.ToArray())
            if rb.Length < 10 then raise (IOException("XGT WORD 읽기 응답이 짧습니다: " + string rb.Length))
            let command = readUInt16LE rb 0
            if command <> 0x0055 then raise (IOException("XGT 읽기 응답 명령 오류: 0x" + command.ToString "X4"))
            let error = readUInt16LE rb 6
            if error <> 0 then
                let detail = if rb.Length >= 10 then readUInt16LE rb 8 else error
                raise (XgtProtocolException("PLC XGT 읽기 오류 0x" + detail.ToString "X4" + " (ErrorStatus=0x" + error.ToString "X4" + ")", detail))

            let blockCount = readUInt16LE rb 8
            let count = min blockCount words.Count
            let mutable pos = 10
            for i in 0 .. count - 1 do
                if pos + 2 > rb.Length then raise (IOException "WORD 응답 길이 부족")
                let dataLen = readUInt16LE rb pos
                pos <- pos + 2
                if dataLen < 1 || pos + dataLen > rb.Length then
                    raise (IOException("WORD 응답 블록 길이 오류: " + string dataLen))
                let value =
                    if dataLen >= 2 then uint16 (int rb.[pos] ||| (int rb.[pos + 1] <<< 8))
                    else uint16 rb.[pos]
                result.[words.[i]] <- value
                pos <- pos + dataLen
            result

    let writeAreaWord (area: char) (word: int) (value: uint16) =
        if isNull stream then raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
        let area = Char.ToUpperInvariant area
        if area <> 'M' && area <> 'D' && area <> 'P' then
            raise (ArgumentException("지원하지 않는 WORD 쓰기 영역: " + string area))

        let name = Encoding.ASCII.GetBytes(Address.toXgtWord area word)
        let body = new MemoryStream()
        body.WriteByte 0x58uy
        body.WriteByte 0x00uy
        body.WriteByte 0x02uy
        body.WriteByte 0x00uy
        body.WriteByte 0x00uy
        body.WriteByte 0x00uy
        body.WriteByte 0x01uy
        body.WriteByte 0x00uy
        let nameLen = uint16LE name.Length
        body.Write(nameLen, 0, nameLen.Length)
        body.Write(name, 0, name.Length)
        body.WriteByte 0x02uy
        body.WriteByte 0x00uy
        body.WriteByte (byte (value &&& 0xFFus))
        body.WriteByte (byte ((value >>> 8) &&& 0xFFus))

        let rb = exchangeWith (sprintf "WRITE WORD %s = %d (0x%04X)" (Address.toXgtWord area word) value value) (body.ToArray())
        if rb.Length < 8 then raise (IOException("XGT WORD 쓰기 응답이 짧습니다: " + string rb.Length))
        let command = readUInt16LE rb 0
        if command <> 0x0059 then raise (IOException("XGT WORD 쓰기 응답 명령 오류: 0x" + command.ToString "X4"))
        let error = readUInt16LE rb 6
        if error <> 0 then
            let detail = if rb.Length >= 10 then readUInt16LE rb 8 else error
            raise (XgtProtocolException("PLC XGT WORD 쓰기 오류 0x" + detail.ToString "X4" + " (ErrorStatus=0x" + error.ToString "X4" + ")", detail))

    /// XGB에서 M 비트 ON/OFF를 확실하게 처리하기 위해
    /// %MX 직접 쓰기 대신 해당 %MW를 읽고 그 비트만 바꿔 WORD로 되쓴다. (v5 TOGGLE FIX)
    /// M01008 -> MW100 bit8, M01009 -> MW100 bit9, M0100F -> MW100 bit15 ...
    let writeMBitByWord (address: string) (value: bool) =
        let b = Address.parseBit address
        if b.Area <> 'M' then raise (ArgumentException("M BIT 전용 함수입니다: " + address))

        let mask = uint16 (1 <<< b.Bit)
        let mutable lastRead = 0us
        let mutable finished = false
        let mutable attempt = 0

        while not finished && attempt < 3 do
            let beforeMap = readAreaWords 'M' [| b.Word |]
            let ok, before = beforeMap.TryGetValue b.Word
            if not ok then raise (IOException(address + " 쓰기 전 MW" + string b.Word + " 읽기 실패"))

            let changed = if value then before ||| mask else before &&& (~~~mask)
            trace
                Note
                (sprintf "%s RMW #%d: %s 0x%04X -> 0x%04X (bit%d %s)" address (attempt + 1) (Address.toXgtWord 'M' b.Word) before changed b.Bit (if value then "SET" else "CLEAR"))
                ""
                0.0
            writeAreaWord 'M' b.Word changed
            Thread.Sleep 20

            let afterMap = readAreaWords 'M' [| b.Word |]
            let ok2, after = afterMap.TryGetValue b.Word
            if not ok2 then raise (IOException(address + " 쓰기 후 MW" + string b.Word + " 읽기 실패"))

            lastRead <- after
            trace
                Note
                (sprintf "%s READBACK #%d: %s = 0x%04X -> bit%d %s" address (attempt + 1) (Address.toXgtWord 'M' b.Word) after b.Bit (if (after &&& mask) <> 0us then "ON" else "OFF"))
                ""
                0.0
            if ((after &&& mask) <> 0us) = value then finished <- true
            else
                Thread.Sleep 20
                attempt <- attempt + 1

        if not finished then
            let lastState = (lastRead &&& mask) <> 0us
            raise (
                IOException(
                    address
                    + " "
                    + (if value then "ON" else "OFF")
                    + " 쓰기 후에도 실제 비트가 "
                    + (if lastState then "ON" else "OFF")
                    + "입니다. PLC 래더에서 같은 M비트를 다시 쓰고 있는지 확인하십시오."
                )
            )

    /// 통신 추적(TX/RX 원문) 알림
    member _.Trace = traceEvent.Publish

    /// 켜면 프레임 바이트까지 추적에 실어 보낸다.
    member _.TraceEnabled
        with get () = traceEnabled
        and set v = traceEnabled <- v

    /// 지금까지 주고받은 프레임 수 / 오류 수
    member _.FrameCount = frameCount
    member _.ErrorCount = errorCount

    member _.ProfileName =
        match profile with
        | Some p -> p.Name
        | None -> "미확정"

    member _.NegotiationLog = negotiationLog

    member _.Connected = not (isNull tcp) && tcp.Connected && not (isNull stream)

    /// r004 프로젝트: XBM-DN32H2(XGB/MK), 내장 FEnet이 Base 0 / Slot 1로 저장되어 있음.
    /// 펌웨어/헤더 처리 차이를 고려해 공식 헤더 조합을 새 TCP 연결마다 자동 시험한다.
    member _.Connect() =
        let log = StringBuilder()
        let mutable last: exn = null
        let mutable connected = false
        let mutable i = 0

        while not connected && i < candidates.Length do
            let sw = Diagnostics.Stopwatch.StartNew()
            try
                profile <- Some candidates.[i]
                connectSocket ()
                probeKnownRead ()
                sw.Stop()
                let line = sprintf "OK   %s  (%s, CPU=0x%02X, Slot=%d, BCC=%s, %.0f ms)" candidates.[i].Name candidates.[i].Company candidates.[i].CpuInfo candidates.[i].Position (if candidates.[i].UseBcc then "사용" else "00") sw.Elapsed.TotalMilliseconds
                log.AppendLine line |> ignore
                trace Note line "" sw.Elapsed.TotalMilliseconds
                negotiationLog <- log.ToString()
                connected <- true
            with ex ->
                sw.Stop()
                last <- ex
                let line = sprintf "FAIL %s  (%.0f ms) : %s" candidates.[i].Name sw.Elapsed.TotalMilliseconds ex.Message
                log.AppendLine line |> ignore
                trace Note line "" sw.Elapsed.TotalMilliseconds
                disposeSocket ()
                i <- i + 1

        if not connected then
            profile <- None
            negotiationLog <- log.ToString()
            raise (
                IOException(
                    "XGT 자동 판별 실패. 통신 로그의 TX/RX 내용을 확인하십시오."
                    + (if isNull last then "" else " 마지막 오류: " + last.Message)
                )
            )

    member _.ReadBits(addresses: IList<string>) : Dictionary<string, bool> =
        if isNull stream then raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
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
        if isNull stream then raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
        let word = Address.parseDWord "읽기" address
        let vals = readAreaWords 'D' [| word |]
        match vals.TryGetValue word with
        | true, v -> v
        | _ -> raise (IOException(address + " 읽기 응답에 데이터가 없습니다."))

    member _.WriteBit(address: string, value: bool) =
        if isNull stream then raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
        let b = Address.parseBit address

        // HMI 스위치에서 사용하는 M영역은 WORD Read-Modify-Write 방식으로 처리한다.
        // 기존 %MX 개별 BIT 쓰기에서 ON은 되지만 OFF가 유지되지 않는 현상을 피한다.
        if b.Area = 'M' then
            writeMBitByWord address value
        else
            let name = Encoding.ASCII.GetBytes(Address.toXgtBit address)
            let body = new MemoryStream()
            body.WriteByte 0x58uy
            body.WriteByte 0x00uy
            body.WriteByte 0x00uy
            body.WriteByte 0x00uy
            body.WriteByte 0x00uy
            body.WriteByte 0x00uy
            body.WriteByte 0x01uy
            body.WriteByte 0x00uy
            let nameLen = uint16LE name.Length
            body.Write(nameLen, 0, nameLen.Length)
            body.Write(name, 0, name.Length)
            body.WriteByte 0x01uy
            body.WriteByte 0x00uy
            body.WriteByte (if value then 0x01uy else 0x00uy)

            let rb = exchangeWith (sprintf "WRITE BIT %s = %s" (Address.toXgtBit address) (if value then "ON" else "OFF")) (body.ToArray())
            if rb.Length < 8 then raise (IOException("XGT BIT 쓰기 응답이 짧습니다: " + string rb.Length))
            let command = readUInt16LE rb 0
            if command <> 0x0059 then raise (IOException("XGT BIT 쓰기 응답 명령 오류: 0x" + command.ToString "X4"))
            let error = readUInt16LE rb 6
            if error <> 0 then
                let detail = if rb.Length >= 10 then readUInt16LE rb 8 else error
                raise (XgtProtocolException("PLC XGT BIT 쓰기 오류 0x" + detail.ToString "X4" + " (ErrorStatus=0x" + error.ToString "X4" + ")", detail))

    member _.WriteWord(address: string, value: uint16) =
        if isNull stream then raise (InvalidOperationException "PLC에 연결되지 않았습니다.")
        let word = Address.parseDWord "쓰기" address
        writeAreaWord 'D' word value

    interface IDisposable with
        member _.Dispose() = disposeSocket ()
