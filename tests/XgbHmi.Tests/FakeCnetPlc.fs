namespace XgbHmi.Tests

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Text
open XgbHmi.Protocol

/// 실제 XGB Cnet(RS-232C / RS-485) 처럼 ASCII 프레임으로 답하는 시험용 PLC.
/// 한 인스턴스가 회선 하나 역할을 하고, 국번별로 메모리를 따로 둔다.
/// (RS-485 다중 접속을 하드웨어 없이 시험하기 위한 것)
type FakeCnetBus() =

    /// 국번별 WORD 메모리. 키는 "1|%MW100" 처럼 국번을 앞에 붙인다.
    let words = ConcurrentDictionary<string, uint16>()
    let requests = ResizeArray<byte[]>()
    let requestLock = obj ()
    /// 응답하지 않을 국번 (선 빠짐 / 전원 꺼짐 흉내)
    let silent = HashSet<int>()

    let key (station: int) (name: string) = string station + "|" + name.ToUpperInvariant()

    let hexOf (text: string) = Convert.ToInt32(text, 16)

    member _.Requests = lock requestLock (fun () -> List.ofSeq requests)

    member _.SetWord(station: int, name: string, value: uint16) = words.[key station name] <- value

    member _.GetWord(station: int, name: string) =
        match words.TryGetValue(key station name) with
        | true, v -> Some v
        | _ -> None

    /// 이 국번은 아무 응답도 하지 않게 만든다.
    member _.Silence(station: int) = silent.Add station |> ignore

    /// 요청 프레임 한 개를 처리하고 응답 프레임을 만든다.
    member this.Exchange(request: byte[], expectBcc: bool) : byte[] =
        lock requestLock (fun () -> requests.Add request)
        let text = Encoding.ASCII.GetString request
        if request.[0] <> CnetFrame.ENQ then failwith "ENQ 로 시작하지 않는 요청"
        let station = hexOf (text.Substring(1, 2))
        if silent.Contains station then raise (TimeoutException "응답 없음 (시험용)")
        let command = text.[3]
        let useBcc = Char.IsUpper command
        if useBcc <> expectBcc then failwith "요청의 BCC 규칙이 예상과 다르다"
        let commandType = text.Substring(4, 2)

        // 소문자 명령(BCC 없음)은 이 시험용 PLC 가 받지 않는다. 실제 모듈처럼 NAK 로 답한다.
        if not useBcc then
            let payload = sprintf "%02X%c%s0011" station command commandType
            let frame =
                Array.append [| CnetFrame.NAK |] (Array.append (Encoding.ASCII.GetBytes payload) [| CnetFrame.ETX |])
            frame
        else
            let payload =
                if Char.ToUpperInvariant command = 'R' then
                    let blocks = hexOf (text.Substring(6, 2))
                    let mutable pos = 8
                    let data = StringBuilder()
                    for _ in 1..blocks do
                        let len = hexOf (text.Substring(pos, 2))
                        pos <- pos + 2
                        let name = text.Substring(pos, len)
                        pos <- pos + len
                        let value =
                            match words.TryGetValue(key station name) with
                            | true, v -> v
                            | _ -> 0us
                        if name.Contains "X" then
                            // 비트 읽기는 1바이트
                            data.Append("01").Append(if value <> 0us then "01" else "00") |> ignore
                        else
                            data.Append("02").Append(value.ToString "X4") |> ignore
                    sprintf "%02XR%s%02X%s" station commandType blocks (data.ToString())
                else
                    let mutable pos = 8
                    let len = hexOf (text.Substring(pos, 2))
                    pos <- pos + 2
                    let name = text.Substring(pos, len)
                    pos <- pos + len
                    if name.Contains "X" then
                        // %MX / %PX 개별 비트 쓰기
                        let value = hexOf (text.Substring(pos, 2))
                        let bitIndex = Int32.Parse(name.Substring 3)
                        let wordName = "%" + string name.[1] + "W" + string (bitIndex / 16)
                        let mask = uint16 (1 <<< (bitIndex % 16))
                        let before =
                            match words.TryGetValue(key station wordName) with
                            | true, v -> v
                            | _ -> 0us
                        words.[key station wordName] <- (if value <> 0 then before ||| mask else before &&& (~~~mask))
                    else
                        words.[key station name] <- uint16 (hexOf (text.Substring(pos, 4)))
                    sprintf "%02XW%s" station commandType

            let frame =
                Array.append [| CnetFrame.ACK |] (Array.append (Encoding.ASCII.GetBytes payload) [| CnetFrame.ETX |])
            Array.append frame (Encoding.ASCII.GetBytes(CnetFrame.bcc frame))

    /// CnetClient 에 끼워 넣을 회선. 여러 국번이 같은 회선을 나눠 쓰는 것도 이것으로 흉내 낸다.
    member this.Transport =
        { new ICnetTransport with
            member _.Exchange(request, expectBcc) = this.Exchange(request, expectBcc)
            member _.Description = "FAKE 9600-8-N-1"
            member _.Dispose() = () }
