namespace XgbHmi.Tests

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading

/// 실제 XGB FEnet 처럼 응답하는 시험용 서버.
/// 클라이언트가 만든 프레임을 그대로 검사할 수 있도록 수신한 요청 본문을 모두 기록한다.
type FakeXgtServer(?company: string) =

    let company = defaultArg company "LSIS-XGT"
    let listener = new TcpListener(IPAddress.Loopback, 0)
    let words = ConcurrentDictionary<string, uint16>()
    let requests = ResizeArray<byte[]>()
    let requestLock = obj ()
    let cts = new CancellationTokenSource()

    let readExact (stream: NetworkStream) (count: int) =
        let buffer = Array.zeroCreate<byte> count
        let mutable offset = 0
        let mutable eof = false
        while not eof && offset < count do
            let n = stream.Read(buffer, offset, count - offset)
            if n <= 0 then eof <- true else offset <- offset + n
        if eof then None else Some buffer

    let u16 (data: byte[]) (offset: int) = int data.[offset] ||| (int data.[offset + 1] <<< 8)

    let responseHeader (bodyLength: int) =
        let header = Array.zeroCreate<byte> 20
        let name = if company = "LGIS-GLOFA" then "LGIS-GLOFA" else "LSIS-XGT\000\000"
        Buffer.BlockCopy(Encoding.ASCII.GetBytes name, 0, header, 0, 10)
        header.[12] <- 0xB0uy
        header.[13] <- 0x11uy // Server -> Client
        header.[16] <- byte (bodyLength &&& 0xFF)
        header.[17] <- byte ((bodyLength >>> 8) &&& 0xFF)
        header

    /// 요청 본문에서 "%MW100" 같은 변수 이름들을 뽑아낸다.
    let parseNames (body: byte[]) (blockCount: int) (start: int) =
        let names = ResizeArray<string>()
        let mutable pos = start
        for _ in 1..blockCount do
            let len = u16 body pos
            pos <- pos + 2
            names.Add(Encoding.ASCII.GetString(body, pos, len))
            pos <- pos + len
        names, pos

    let handle (body: byte[]) =
        lock requestLock (fun () -> requests.Add body)
        let command = u16 body 0
        let dataType = u16 body 2

        match command with
        | 0x0054 ->
            // 읽기 요청 -> 각 블록에 대해 2바이트 데이터 응답
            let blockCount = u16 body 6
            let names, _ = parseNames body blockCount 8
            let payload = ResizeArray<byte>()
            payload.AddRange [| 0x55uy; 0x00uy |]
            payload.AddRange [| byte (dataType &&& 0xFF); byte ((dataType >>> 8) &&& 0xFF) |]
            payload.AddRange [| 0x00uy; 0x00uy |] // reserved
            payload.AddRange [| 0x00uy; 0x00uy |] // error status
            payload.AddRange [| byte (blockCount &&& 0xFF); byte ((blockCount >>> 8) &&& 0xFF) |]
            for name in names do
                let value = match words.TryGetValue name with | true, v -> v | _ -> 0us
                payload.AddRange [| 0x02uy; 0x00uy |]
                payload.Add(byte (value &&& 0xFFus))
                payload.Add(byte ((value >>> 8) &&& 0xFFus))
            payload.ToArray()

        | 0x0058 ->
            // 쓰기 요청 -> 메모리에 반영하고 성공 응답
            let blockCount = u16 body 6
            let names, pos = parseNames body blockCount 8
            let mutable p = pos
            for name in names do
                let dataLen = u16 body p
                p <- p + 2
                if dataLen >= 2 then
                    words.[name] <- uint16 (int body.[p] ||| (int body.[p + 1] <<< 8))
                elif dataLen = 1 then
                    words.[name] <- uint16 body.[p]
                p <- p + dataLen
            [| 0x59uy; 0x00uy
               byte (dataType &&& 0xFF); byte ((dataType >>> 8) &&& 0xFF)
               0x00uy; 0x00uy
               0x00uy; 0x00uy |]

        | _ ->
            [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy |]

    let serve () =
        let rec loop () =
            if not cts.IsCancellationRequested then
                let client =
                    try Some(listener.AcceptTcpClient())
                    with _ -> None
                match client with
                | None -> ()
                | Some client ->
                    let thread =
                        Thread(fun () ->
                            use client = client
                            use stream = client.GetStream()
                            let mutable alive = true
                            while alive do
                                match readExact stream 20 with
                                | None -> alive <- false
                                | Some header ->
                                    let bodyLength = u16 header 16
                                    match readExact stream bodyLength with
                                    | None -> alive <- false
                                    | Some body ->
                                        let response = handle body
                                        stream.Write(responseHeader response.Length, 0, 20)
                                        stream.Write(response, 0, response.Length)
                                        stream.Flush())
                    thread.IsBackground <- true
                    thread.Start()
                    loop ()
        loop ()

    do
        listener.Start()
        let t = Thread(ThreadStart serve)
        t.IsBackground <- true
        t.Start()

    member _.Port = (listener.LocalEndpoint :?> IPEndPoint).Port

    member _.SetWord(name: string, value: uint16) = words.[name] <- value

    member _.GetWord(name: string) =
        match words.TryGetValue name with
        | true, v -> Some v
        | _ -> None

    /// 클라이언트가 보낸 요청 본문 목록 (프레임 검증용)
    member _.Requests = lock requestLock (fun () -> List.ofSeq requests)

    interface IDisposable with
        member _.Dispose() =
            cts.Cancel()
            try listener.Stop() with _ -> ()
