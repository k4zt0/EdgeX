module XgbHmi.Tests.XgtClientTests

open System
open System.Text
open Xunit
open XgbHmi.Protocol
open XgbHmi.Tests

let private connect (server: FakeXgtServer) =
    let client = new XgtClient("127.0.0.1", server.Port, 2000)
    client.Connect()
    client

let private bodyText (body: byte[]) = Encoding.ASCII.GetString body

[<Fact>]
let ``연결하면 %MW0 시험 프레임을 먼저 보낸다`` () =
    use server = new FakeXgtServer()
    use client = connect server
    let requests = server.Requests
    Assert.NotEmpty requests

    let probe = requests.Head
    // Read / WORD / 1 block / %MW0  (v6 과 바이트 단위로 동일)
    Assert.Equal<byte[]>(
        [| 0x54uy; 0x00uy; 0x02uy; 0x00uy; 0x00uy; 0x00uy; 0x01uy; 0x00uy
           0x04uy; 0x00uy; 0x25uy; 0x4Duy; 0x57uy; 0x30uy |],
        probe)
    Assert.Equal("XGB(MK) / Slot1 / BCC", client.ProfileName)

[<Fact>]
let ``M 비트는 해당 WORD 의 비트로 읽는다`` () =
    use server = new FakeXgtServer()
    // MW100 bit8 = M01008 만 ON
    server.SetWord("%MW100", 0x0100us)
    use client = connect server

    let values = client.ReadBits [| "M01008"; "M01009"; "M0100F" |]
    Assert.True values.["M01008"]
    Assert.False values.["M01009"]
    Assert.False values.["M0100F"]

[<Fact>]
let ``P 비트도 WORD 단위로 묶어 읽는다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%PW12", 0x0001us) // P00120
    use client = connect server

    let values = client.ReadBits [| "P00120"; "P00121" |]
    Assert.True values.["P00120"]
    Assert.False values.["P00121"]

[<Fact>]
let ``같은 WORD 의 비트 여러 개는 한 번만 읽는다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0xFFFFus)
    use client = connect server
    let before = server.Requests.Length

    client.ReadBits [| "M01000"; "M01001"; "M01002"; "M0100F" |] |> ignore

    // 프레임 1개만 늘어나야 한다.
    Assert.Equal(before + 1, server.Requests.Length)

[<Fact>]
let ``M 비트 쓰기는 WORD 읽고-고치고-쓰기 방식이다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x00FFus) // bit0..7 ON
    use client = connect server

    client.WriteBit("M01008", true) // bit8 만 추가로 ON

    // 다른 비트를 건드리지 않고 bit8 만 세워야 한다.
    Assert.Equal(Some 0x01FFus, server.GetWord "%MW100")

    client.WriteBit("M01008", false)
    Assert.Equal(Some 0x00FFus, server.GetWord "%MW100")

[<Fact>]
let ``P 비트 쓰기는 %PX 개별 비트 프레임을 쓴다`` () =
    use server = new FakeXgtServer()
    use client = connect server

    client.WriteBit("P00120", true)

    let last = server.Requests |> List.last
    Assert.Equal(0x58, int last.[0] ||| (int last.[1] <<< 8)) // Write
    Assert.Equal(0x00, int last.[2] ||| (int last.[3] <<< 8)) // BIT type
    Assert.Contains("%PX192", bodyText last)

[<Fact>]
let ``D WORD 는 읽고 쓸 수 있다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%DW100", 1234us)
    use client = connect server

    Assert.Equal(1234us, client.ReadWord "D100")

    client.WriteWord("D200", 4321us)
    Assert.Equal(Some 4321us, server.GetWord "%DW200")
    Assert.Equal(4321us, client.ReadWord "D200")

[<Fact>]
let ``WORD 쓰기 프레임은 2바이트 데이터를 리틀엔디안으로 담는다`` () =
    use server = new FakeXgtServer()
    use client = connect server

    client.WriteWord("D200", 0x1234us)

    let last = server.Requests |> List.last
    Assert.Equal(0x58, int last.[0] ||| (int last.[1] <<< 8))
    Assert.Equal(0x02, int last.[2] ||| (int last.[3] <<< 8)) // WORD type
    Assert.Equal(0x34uy, last.[last.Length - 2])
    Assert.Equal(0x12uy, last.[last.Length - 1])

[<Fact>]
let ``한 프레임에 17 WORD 이상은 요청하지 않는다`` () =
    use server = new FakeXgtServer()
    use client = connect server
    let addresses = [| for i in 0..16 -> sprintf "M%05d" (i * 16) |]
    Assert.ThrowsAny<exn>(fun () -> client.ReadBits addresses |> ignore)

[<Fact>]
let ``GLOFA 헤더만 받는 PLC 와도 연결된다`` () =
    use server = new FakeXgtServer("LGIS-GLOFA")
    use client = connect server
    Assert.True client.Connected

[<Fact>]
let ``추적을 켜면 TX RX 원문이 그대로 나온다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x0100us)

    let client = new XgtClient("127.0.0.1", server.Port, 2000)
    let traces = ResizeArray<XgtTrace>()
    client.Trace.Add traces.Add
    client.TraceEnabled <- true
    client.Connect()
    use client = client

    client.ReadBits [| "M01008" |] |> ignore

    let tx = traces |> Seq.filter (fun t -> t.Kind = Tx) |> List.ofSeq
    let rx = traces |> Seq.filter (fun t -> t.Kind = Rx) |> List.ofSeq

    Assert.NotEmpty tx
    Assert.NotEmpty rx
    // 요약에 실제 읽은 직접변수가 들어 있어야 한다.
    Assert.Contains(tx, fun t -> t.Summary.Contains "%MW100")
    // TX/RX 원문은 16진수 바이트로 남는다.
    Assert.All(tx, fun t -> Assert.Matches(@"^[0-9A-F]{2}( [0-9A-F]{2})+$", t.Hex))
    // 응답에는 명령 코드와 상태가 함께 기록된다.
    Assert.Contains(rx, fun t -> t.Summary.Contains "cmd=0x0055" && t.Summary.Contains "status=0x0000")
    Assert.True(rx |> List.forall (fun t -> t.ElapsedMs >= 0.0))

[<Fact>]
let ``추적을 끄면 프레임 원문을 만들지 않는다`` () =
    use server = new FakeXgtServer()
    let client = new XgtClient("127.0.0.1", server.Port, 2000)
    let traces = ResizeArray<XgtTrace>()
    client.Trace.Add traces.Add
    client.Connect()
    use client = client
    client.ReadBits [| "M01008" |] |> ignore

    Assert.Empty(traces |> Seq.filter (fun t -> t.Kind = Tx || t.Kind = Rx))
    // 연결 시도 기록(Note)은 항상 남는다.
    Assert.Contains(traces, fun t -> t.Kind = Note && t.Summary.StartsWith "OK")

[<Fact>]
let ``M 비트 쓰기는 읽고-고치고-쓰기 과정을 추적에 남긴다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x00FFus)
    let client = new XgtClient("127.0.0.1", server.Port, 2000)
    let traces = ResizeArray<XgtTrace>()
    client.Trace.Add traces.Add
    client.Connect()
    use client = client

    client.WriteBit("M01008", true)

    let notes = traces |> Seq.filter (fun t -> t.Kind = Note) |> List.ofSeq
    Assert.Contains(notes, fun t -> t.Summary.Contains "RMW #1" && t.Summary.Contains "0x00FF -> 0x01FF")
    Assert.Contains(notes, fun t -> t.Summary.Contains "READBACK #1" && t.Summary.Contains "bit8 ON")

[<Fact>]
let ``프레임 수와 오류 수를 센다`` () =
    use server = new FakeXgtServer()
    use client = connect server
    let before = client.FrameCount
    client.ReadBits [| "M01008" |] |> ignore
    client.ReadWord "D100" |> ignore
    Assert.Equal(before + 2L, client.FrameCount)
    Assert.Equal(0L, client.ErrorCount)
