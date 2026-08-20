module XgbHmi.Tests.CnetTests

open System
open System.Text
open Xunit
open XgbHmi.Protocol
open XgbHmi.Tests

let private ascii (frame: byte[]) = Encoding.ASCII.GetString frame

let private settings (station: int) =
    { PortName = "FAKE"
      Baud = 9600
      DataBits = 8
      Parity = ParityNone
      StopBits = 1
      Station = station
      TimeoutMs = 500
      HalfDuplex = true }

let private connect (bus: FakeCnetBus) (station: int) =
    let client = new CnetClient(settings station, fun _ -> bus.Transport)
    client.Connect()
    client

[<Fact>]
let ``개별 읽기 프레임은 XGT Cnet 규격대로 만든다`` () =
    let frame = CnetFrame.readFrame true 1 [ "%MW100" ]
    // ENQ + 국번(01) + R + SS + 블록수(01) + 이름길이(06) + %MW100 + EOT + BCC
    Assert.Equal(CnetFrame.ENQ, frame.[0])
    Assert.Equal("01RSS0106%MW100", ascii frame.[1..15])
    Assert.Equal(CnetFrame.EOT, frame.[16])
    // BCC 는 머리부터 꼬리까지 더한 하위 1바이트
    Assert.Equal(CnetFrame.bcc frame.[0..16], ascii frame.[17..18])

[<Fact>]
let ``BCC 를 쓰지 않는 프레임은 명령 글자만 소문자로 보낸다`` () =
    let frame = CnetFrame.readFrame false 0 [ "%MW0" ]
    // 명령 글자(r)만 소문자다. 명령 형식(SS)은 그대로 두고 BCC 도 붙이지 않는다. (XGT Cnet 규칙)
    Assert.Equal("00rSS0104%MW0", ascii frame.[1 .. frame.Length - 2])
    Assert.Equal(CnetFrame.EOT, frame.[frame.Length - 1])

[<Fact>]
let ``WORD 쓰기 프레임은 값을 16진수 4자로 담는다`` () =
    let frame = CnetFrame.writeWordFrame true 2 "%DW200" 0x1234us
    Assert.Equal("02WSS0106%DW2001234", ascii frame.[1 .. frame.Length - 4])

[<Fact>]
let ``BIT 쓰기 프레임은 값을 16진수 2자로 담는다`` () =
    let frame = CnetFrame.writeBitFrame true 0 "%PX192" true
    Assert.Equal("00WSS0106%PX19201", ascii frame.[1 .. frame.Length - 4])

[<Fact>]
let ``한 프레임에 17블록 이상은 만들지 않는다`` () =
    let names = [ for i in 0..16 -> sprintf "%%MW%d" i ]
    Assert.ThrowsAny<exn>(fun () -> CnetFrame.readFrame true 0 names |> ignore)

[<Fact>]
let ``응답 국번이 다르면 오류로 본다`` () =
    let body =
        Array.append [| CnetFrame.ACK |] (Array.append (Encoding.ASCII.GetBytes "02RSS01021234") [| CnetFrame.ETX |])
    let frame = Array.append body (Encoding.ASCII.GetBytes(CnetFrame.bcc body))
    let ex = Assert.ThrowsAny<exn>(fun () -> CnetFrame.parse true 1 'R' frame |> ignore)
    Assert.Contains("국번", ex.Message)

[<Fact>]
let ``NAK 응답은 PLC 오류 코드를 그대로 올린다`` () =
    let body =
        Array.append [| CnetFrame.NAK |] (Array.append (Encoding.ASCII.GetBytes "01RSS0011") [| CnetFrame.ETX |])
    let frame = Array.append body (Encoding.ASCII.GetBytes(CnetFrame.bcc body))
    let ex = Assert.ThrowsAny<exn>(fun () -> CnetFrame.parse true 1 'R' frame |> ignore)
    match ex with
    | XgtProtocolException(_, code) -> Assert.Equal(0x0011, code)
    | other -> failwith ("PLC 오류로 올리지 않았다: " + other.Message)

[<Fact>]
let ``BCC 가 어긋난 응답은 받지 않는다`` () =
    let body =
        Array.append [| CnetFrame.ACK |] (Array.append (Encoding.ASCII.GetBytes "01RSS01021234") [| CnetFrame.ETX |])
    let frame = Array.append body (Encoding.ASCII.GetBytes "00")
    let ex = Assert.ThrowsAny<exn>(fun () -> CnetFrame.parse true 1 'R' frame |> ignore)
    Assert.Contains("BCC", ex.Message)

[<Fact>]
let ``직렬에서도 M 비트는 해당 WORD 의 비트로 읽는다`` () =
    let bus = FakeCnetBus()
    bus.SetWord(1, "%MW100", 0x0100us) // M01008 만 ON
    use client = connect bus 1

    let values = client.ReadBits [| "M01008"; "M01009"; "M0100F" |]
    Assert.True values.["M01008"]
    Assert.False values.["M01009"]
    Assert.False values.["M0100F"]

[<Fact>]
let ``직렬 M 비트 쓰기도 읽고-고치고-쓰기 방식이다`` () =
    let bus = FakeCnetBus()
    bus.SetWord(1, "%MW100", 0x00FFus) // bit0..7 ON
    use client = connect bus 1

    client.WriteBit("M01008", true)
    // 다른 비트를 건드리지 않고 bit8 만 세워야 한다.
    Assert.Equal(Some 0x01FFus, bus.GetWord(1, "%MW100"))

    client.WriteBit("M01008", false)
    Assert.Equal(Some 0x00FFus, bus.GetWord(1, "%MW100"))

[<Fact>]
let ``직렬 P 비트 쓰기는 PX 개별 비트 프레임을 쓴다`` () =
    let bus = FakeCnetBus()
    use client = connect bus 0

    client.WriteBit("P00120", true)
    let last = bus.Requests |> List.last |> ascii
    Assert.Contains("%PX192", last)
    Assert.Equal(Some 0x0001us, bus.GetWord(0, "%PW12"))

[<Fact>]
let ``직렬에서 D WORD 를 읽고 쓸 수 있다`` () =
    let bus = FakeCnetBus()
    bus.SetWord(3, "%DW100", 1234us)
    use client = connect bus 3

    Assert.Equal(1234us, client.ReadWord "D100")
    client.WriteWord("D200", 4321us)
    Assert.Equal(Some 4321us, bus.GetWord(3, "%DW200"))

[<Fact>]
let ``RS-485 한 회선에 국번이 다른 여러 대를 붙일 수 있다`` () =
    let bus = FakeCnetBus()
    bus.SetWord(1, "%MW100", 0x0100us) // 1호기: M01008 ON
    bus.SetWord(2, "%MW100", 0x0000us) // 2호기: OFF
    use first = connect bus 1
    use second = connect bus 2

    Assert.True((first.ReadBits [| "M01008" |]).["M01008"])
    Assert.False((second.ReadBits [| "M01008" |]).["M01008"])

    // 2호기에 쓴 값이 1호기로 새지 않는다.
    second.WriteBit("M01008", true)
    Assert.Equal(Some 0x0100us, bus.GetWord(1, "%MW100"))
    Assert.Equal(Some 0x0100us, bus.GetWord(2, "%MW100"))
    Assert.Equal(1, first.Station)
    Assert.Equal(2, second.Station)

[<Fact>]
let ``직렬도 연결하면 MW0 시험 프레임을 먼저 보낸다`` () =
    let bus = FakeCnetBus()
    use client = connect bus 5
    let first = bus.Requests |> List.head |> ascii
    Assert.Equal("05RSS0104%MW0", first.Substring(1, 13))
    Assert.Contains("국번 5", client.ProfileName)
    Assert.Contains("BCC 사용", client.ProfileName)

[<Fact>]
let ``응답이 없는 국번은 연결 실패로 알려 준다`` () =
    let bus = FakeCnetBus()
    bus.Silence 7
    let client = new CnetClient(settings 7, fun _ -> bus.Transport)
    let ex = Assert.ThrowsAny<exn>(fun () -> client.Connect())
    Assert.Contains("국번 7", ex.Message)
    Assert.False client.Connected
    (client :> IDisposable).Dispose()

[<Fact>]
let ``직렬 추적을 켜면 TX RX 원문이 그대로 나온다`` () =
    let bus = FakeCnetBus()
    bus.SetWord(1, "%MW100", 0x0100us)
    let client = new CnetClient(settings 1, fun _ -> bus.Transport)
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
    Assert.Contains(tx, fun t -> t.Summary.Contains "%MW100")
    Assert.All(tx, fun t -> Assert.Matches(@"^[0-9A-F]{2}( [0-9A-F]{2})+$", t.Hex))
    Assert.Contains(rx, fun t -> t.Summary.Contains "ACK")

[<Fact>]
let ``직렬 M 비트 쓰기 과정도 추적에 남는다`` () =
    let bus = FakeCnetBus()
    bus.SetWord(1, "%MW100", 0x00FFus)
    let client = new CnetClient(settings 1, fun _ -> bus.Transport)
    let traces = ResizeArray<XgtTrace>()
    client.Trace.Add traces.Add
    client.Connect()
    use client = client

    client.WriteBit("M01008", true)

    let notes = traces |> Seq.filter (fun t -> t.Kind = Note) |> List.ofSeq
    Assert.Contains(notes, fun t -> t.Summary.Contains "RMW #1" && t.Summary.Contains "0x00FF -> 0x01FF")
    Assert.Contains(notes, fun t -> t.Summary.Contains "READBACK #1" && t.Summary.Contains "bit8 ON")

[<Fact>]
let ``직렬도 프레임 수와 오류 수를 센다`` () =
    let bus = FakeCnetBus()
    use client = connect bus 1
    let before = client.FrameCount
    client.ReadBits [| "M01008" |] |> ignore
    client.ReadWord "D100" |> ignore
    Assert.Equal(before + 2L, client.FrameCount)
    Assert.Equal(0L, client.ErrorCount)
