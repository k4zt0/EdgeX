/// 시험용 가짜 XGB/XGT PLC.
///
/// 실제 설비 없이 운전 화면과 HMI 화면을 움직여 보기 위한 도구다. 배포에는 포함되지 않는다.
/// 프로젝트 XML을 읽어 스위치의 `디바이스(M) -> 상태확인 디바이스(P)` 대응을 그대로 흉내 내고,
/// r004 래더에 적힌 `MOV D200 D100` 도 함께 돌린다.
///
///   dotnet run --project tools/FakePlc -- [프로젝트XML] [포트]
///
/// 앱에서 PLC IP 를 127.0.0.1 로 두고 연결하면 된다.
module FakePlc.Program

open System
open System.Collections.Concurrent
open System.Net
open System.Net.Sockets
open System.Text
open System.Threading
open XgbHmi.Core
open XgbHmi.Protocol

/// PLC 메모리. XGT 직접변수 이름("%MW100") 그대로 담는다.
let private words = ConcurrentDictionary<string, uint16>()

let private getWord (name: string) =
    match words.TryGetValue name with
    | true, v -> v
    | _ -> 0us

let private setBit (name: string) (bit: int) (value: bool) =
    let mask = uint16 (1 <<< bit)
    let before = getWord name
    let after = if value then before ||| mask else before &&& ~~~mask
    if after <> before then words.[name] <- after

let private getBit (name: string) (bit: int) =
    (getWord name &&& uint16 (1 <<< bit)) <> 0us

// ---------------------------------------------------------------------------
//  래더 흉내
// ---------------------------------------------------------------------------

/// 스위치 한 개의 대응: 조작 비트(M) -> 상태확인 비트(P)
type private Link =
    { MWord: string
      MBit: int
      PWord: string
      PBit: int
      Name: string }

let private buildLinks (project: HmiProject) =
    project.Items
    |> List.choose (fun item ->
        if not (ItemKind.isBit item.Kind) then None
        elif String.IsNullOrWhiteSpace item.Device || String.IsNullOrWhiteSpace item.MonitorDevice then None
        else
            try
                let m = Address.parseBit item.Device
                let p = Address.parseBit item.MonitorDevice
                Some
                    { MWord = Address.toXgtWord m.Area m.Word
                      MBit = m.Bit
                      PWord = Address.toXgtWord p.Area p.Word
                      PBit = p.Bit
                      Name = item.Name }
            with _ -> None)

/// 실제 래더가 하는 일을 흉내 낸다.
/// 1) 조작 비트(M)를 상태확인 비트(P)로 옮긴다  2) MOV D200 D100
let private runLadder (links: Link list) =
    for link in links do
        setBit link.PWord link.PBit (getBit link.MWord link.MBit)
    words.["%DW100"] <- getWord "%DW200"

// ---------------------------------------------------------------------------
//  XGT FEnet 프레임 처리
// ---------------------------------------------------------------------------

let private u16 (data: byte[]) (offset: int) = int data.[offset] ||| (int data.[offset + 1] <<< 8)

let private responseHeader (bodyLength: int) =
    let header = Array.zeroCreate<byte> 20
    Buffer.BlockCopy(Encoding.ASCII.GetBytes "LSIS-XGT\000\000", 0, header, 0, 10)
    header.[12] <- 0xB0uy
    header.[13] <- 0x11uy // Server -> Client
    header.[16] <- byte (bodyLength &&& 0xFF)
    header.[17] <- byte ((bodyLength >>> 8) &&& 0xFF)
    header

let private readExact (stream: NetworkStream) (count: int) =
    let buffer = Array.zeroCreate<byte> count
    let mutable offset = 0
    let mutable eof = false
    while not eof && offset < count do
        let n = stream.Read(buffer, offset, count - offset)
        if n <= 0 then eof <- true else offset <- offset + n
    if eof then None else Some buffer

/// 요청 본문에서 "%MW100" 같은 변수 이름들을 뽑아낸다.
let private parseNames (body: byte[]) (blockCount: int) (start: int) =
    let names = ResizeArray<string>()
    let mutable pos = start
    for _ in 1..blockCount do
        let len = u16 body pos
        pos <- pos + 2
        names.Add(Encoding.ASCII.GetString(body, pos, len))
        pos <- pos + len
    names, pos

let private handle (links: Link list) (verbose: bool) (body: byte[]) =
    let command = u16 body 0
    let dataType = u16 body 2

    match command with
    | 0x0054 ->
        let blockCount = u16 body 6
        let names, _ = parseNames body blockCount 8
        let payload = ResizeArray<byte>()
        payload.AddRange [| 0x55uy; 0x00uy |]
        payload.AddRange [| byte (dataType &&& 0xFF); byte ((dataType >>> 8) &&& 0xFF) |]
        payload.AddRange [| 0x00uy; 0x00uy |] // reserved
        payload.AddRange [| 0x00uy; 0x00uy |] // error status
        payload.AddRange [| byte (blockCount &&& 0xFF); byte ((blockCount >>> 8) &&& 0xFF) |]
        for name in names do
            let value = getWord name
            payload.AddRange [| 0x02uy; 0x00uy |]
            payload.Add(byte (value &&& 0xFFus))
            payload.Add(byte ((value >>> 8) &&& 0xFFus))
        payload.ToArray()

    | 0x0058 ->
        let blockCount = u16 body 6
        let names, pos = parseNames body blockCount 8
        let mutable p = pos
        for name in names do
            let dataLen = u16 body p
            p <- p + 2
            let value =
                if dataLen >= 2 then uint16 (int body.[p] ||| (int body.[p + 1] <<< 8))
                elif dataLen = 1 then uint16 body.[p]
                else 0us
            // %MX / %PX 같은 개별 비트 쓰기도 받아 준다.
            if name.Length > 2 && (name.[2] = 'X' || name.[2] = 'x') then
                match Int32.TryParse(name.Substring 3) with
                | true, index ->
                    let area = name.[1]
                    setBit (Address.toXgtWord area (index / 16)) (index % 16) (value <> 0us)
                | _ -> ()
            else
                words.[name] <- value
            if verbose then printfn "  WRITE %s = %d (0x%04X)" name value value
            p <- p + dataLen
        // 쓰기가 들어오면 곧바로 래더를 한 번 돌린다. (다음 스캔에서 P가 따라온다)
        runLadder links
        [| 0x59uy; 0x00uy
           byte (dataType &&& 0xFF); byte ((dataType >>> 8) &&& 0xFF)
           0x00uy; 0x00uy
           0x00uy; 0x00uy |]

    | _ -> [| 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0x00uy; 0xFFuy; 0xFFuy |]

// ---------------------------------------------------------------------------

[<EntryPoint>]
let main argv =
    let verbose = argv |> Array.contains "-v"
    let argv = argv |> Array.filter (fun a -> a <> "-v")
    let projectPath = if argv.Length > 0 then argv.[0] else ProjectIo.defaultProjectPath ()
    let port = if argv.Length > 1 then int argv.[1] else Limits.defaultPort

    let project = ProjectIo.loadOrDefault projectPath
    let links = buildLinks project

    printfn "가짜 PLC — 시험 전용 (실제 설비 아님)"
    printfn "프로젝트 : %s" projectPath
    printfn "래더 흉내 : M 비트 %d개 -> P 상태확인 비트, MOV D200 D100" links.Length

    // 처음부터 값이 보이도록 몇 가지를 채워 둔다.
    words.["%DW200"] <- 250us
    runLadder links

    let listener = new TcpListener(IPAddress.Loopback, port)
    listener.Start()
    printfn "듣는 중 : 127.0.0.1:%d" port
    printfn ""
    printfn "앱에서 PLC IP 를 127.0.0.1 로 두고 [연결] 을 누르십시오."
    printfn "끝내려면 Ctrl+C."

    // 래더는 계속 돈다. (다른 곳에서 값이 바뀌어도 P가 따라오도록)
    let scan =
        Thread(fun () ->
            while true do
                runLadder links
                Thread.Sleep 100)
    scan.IsBackground <- true
    scan.Start()

    while true do
        let client = listener.AcceptTcpClient()
        let thread =
            Thread(fun () ->
                use client = client
                printfn "접속됨 : %O" client.Client.RemoteEndPoint
                try
                    use stream = client.GetStream()
                    let mutable alive = true
                    while alive do
                        match readExact stream 20 with
                        | None -> alive <- false
                        | Some header ->
                            match readExact stream (u16 header 16) with
                            | None -> alive <- false
                            | Some body ->
                                let response = handle links verbose body
                                stream.Write(responseHeader response.Length, 0, 20)
                                stream.Write(response, 0, response.Length)
                                stream.Flush()
                with _ -> ()
                printfn "접속 끊김")
        thread.IsBackground <- true
        thread.Start()
    0
