module XgbHmi.Tests.MultiPlcTests

open System
open System.Collections.Concurrent
open System.IO
open System.Threading
open Xunit
open XgbHmi.Core
open XgbHmi.App.Services
open XgbHmi.Tests

let private tempFile () =
    Path.Combine(Path.GetTempPath(), "xgbhmi_plc_" + Guid.NewGuid().ToString("N") + ".xml")

let private waitFor (timeoutMs: int) (check: unit -> bool) =
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable ok = check ()
    while not ok && sw.ElapsedMilliseconds < int64 timeoutMs do
        Thread.Sleep 25
        ok <- check ()
    ok

// ---------------------------------------------------------------------------
//  프로젝트 파일 (v6 호환 + PLC 목록)
// ---------------------------------------------------------------------------

[<Fact>]
let ``PLC 목록이 없는 v6 파일은 이더넷 한 대로 읽는다`` () =
    let path = tempFile ()
    try
        File.WriteAllText(
            path,
            """<?xml version="1.0" encoding="utf-8"?>
<HmiProject>
  <PlcIp>192.168.0.50</PlcIp>
  <Port>2004</Port>
  <CycleMs>250</CycleMs>
  <Items>
    <HmiItem><Type>SWITCH</Type><Name>a</Name><Device>M1000</Device></HmiItem>
  </Items>
</HmiProject>"""
        )
        let project = ProjectIo.load path
        Assert.Single project.Plcs |> ignore
        let only = project.Plcs.Head
        Assert.Equal(LinkEthernet, only.Kind)
        Assert.Equal("192.168.0.50", only.Ip)
        Assert.Equal(2004, only.Port)
        Assert.Equal(250, only.CycleMs)
        Assert.Equal("PLC1", only.Id)
        // 요소는 그 한 대를 쓰도록 이름표가 채워진다.
        Assert.Equal("PLC1", project.Items.Head.PlcId)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``이더넷과 직렬을 섞은 PLC 목록을 저장하고 다시 읽는다`` () =
    let path = tempFile ()
    try
        let plcs =
            [ { PlcLink.ethernet "PLC1" with Name = "1호기"; Ip = "192.168.1.120" }
              { PlcLink.serial "PLC2" LinkRs232 0 with
                  Name = "2호기"
                  SerialPort = "COM3"
                  Baud = 38400
                  Parity = "EVEN"
                  StopBits = 2
                  DataBits = 7
                  CycleMs = 500 }
              { PlcLink.serial "PLC3" LinkRs485 5 with Name = "3호기"; SerialPort = "COM4"; Enabled = false } ]

        let project =
            { Project.createDefault () with
                Plcs = plcs
                Items =
                    [ { Item.create Switch with Device = "M1000"; PlcId = "PLC2" }
                      { Item.create Lamp with Device = "P00000"; PlcId = "PLC3" } ] }

        ProjectIo.save path project
        let reloaded = ProjectIo.load path

        Assert.Equal(3, reloaded.Plcs.Length)
        let serial = reloaded.Plcs.[1]
        Assert.Equal("PLC2", serial.Id)
        Assert.Equal("2호기", serial.Name)
        Assert.Equal(LinkRs232, serial.Kind)
        Assert.Equal("COM3", serial.SerialPort)
        Assert.Equal(38400, serial.Baud)
        Assert.Equal("EVEN", serial.Parity)
        Assert.Equal(2, serial.StopBits)
        Assert.Equal(7, serial.DataBits)
        Assert.Equal(500, serial.CycleMs)

        let rs485 = reloaded.Plcs.[2]
        Assert.Equal(LinkRs485, rs485.Kind)
        Assert.Equal(5, rs485.Station)
        Assert.False rs485.Enabled

        // 요소가 어느 PLC 를 쓰는지도 그대로 남는다.
        Assert.Equal("PLC2", reloaded.Items.Head.PlcId)
        Assert.Equal("PLC3", reloaded.Items.[1].PlcId)

        // v6 가 읽는 자리에는 첫 이더넷 PLC 가 그대로 들어 있다.
        Assert.Equal("192.168.1.120", reloaded.PlcIp)
        Assert.Equal(2004, reloaded.Port)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``없어진 PLC 를 가리키는 요소는 첫 번째 PLC 로 옮긴다`` () =
    let project =
        { Project.empty with
            Plcs = [ PlcLink.ethernet "PLC1" ]
            Items = [ { Item.create Switch with Device = "M1000"; PlcId = "PLC9" } ] }
        |> Project.normalizeLinks
    Assert.Equal("PLC1", project.Items.Head.PlcId)

[<Fact>]
let ``이름표가 겹치면 새 이름표를 준다`` () =
    let project =
        { Project.empty with Plcs = [ PlcLink.ethernet "PLC1"; PlcLink.ethernet "PLC1" ] }
        |> Project.normalizeLinks
    Assert.Equal<string list>([ "PLC1"; "PLC2" ], project.Plcs |> List.map (fun l -> l.Id))

// ---------------------------------------------------------------------------
//  PLC 목록 검사
// ---------------------------------------------------------------------------

[<Fact>]
let ``같은 회선에 같은 국번을 두 번 쓰면 막는다`` () =
    let plcs =
        [ { PlcLink.serial "PLC1" LinkRs485 1 with SerialPort = "COM3" }
          { PlcLink.serial "PLC2" LinkRs485 1 with SerialPort = "COM3" } ]
    match Project.validatePlcs plcs with
    | Error m -> Assert.Contains("국번", m)
    | Ok() -> failwith "국번이 겹쳤는데 통과했다"

[<Fact>]
let ``같은 회선인데 통신 속도가 다르면 막는다`` () =
    let plcs =
        [ { PlcLink.serial "PLC1" LinkRs485 1 with SerialPort = "COM3"; Baud = 9600 }
          { PlcLink.serial "PLC2" LinkRs485 2 with SerialPort = "COM3"; Baud = 38400 } ]
    match Project.validatePlcs plcs with
    | Error m -> Assert.Contains("통신 속도", m)
    | Ok() -> failwith "회선 설정이 달랐는데 통과했다"

[<Fact>]
let ``다른 회선이면 국번이 같아도 된다`` () =
    let plcs =
        [ { PlcLink.serial "PLC1" LinkRs485 1 with SerialPort = "COM3" }
          { PlcLink.serial "PLC2" LinkRs485 1 with SerialPort = "COM4" } ]
    Assert.True(match Project.validatePlcs plcs with Ok() -> true | Error _ -> false)

[<Fact>]
let ``직렬 포트를 비워 두면 막는다`` () =
    let plcs = [ { PlcLink.serial "PLC1" LinkRs232 0 with SerialPort = "" } ]
    match Project.validatePlcs plcs with
    | Error m -> Assert.Contains("직렬 포트", m)
    | Ok() -> failwith "포트가 비었는데 통과했다"

[<Fact>]
let ``사용으로 둔 PLC 가 하나도 없으면 막는다`` () =
    let plcs = [ { PlcLink.ethernet "PLC1" with Enabled = false } ]
    Assert.True(match Project.validatePlcs plcs with Error _ -> true | Ok() -> false)

// ---------------------------------------------------------------------------
//  폴링 목록 나누기
// ---------------------------------------------------------------------------

[<Fact>]
let ``폴링 목록은 PLC 별로 나뉜다`` () =
    let plcs = [ PlcLink.ethernet "PLC1"; { PlcLink.serial "PLC2" LinkRs485 1 with SerialPort = "COM3" } ]
    let items =
        [ { Item.create Switch with Device = "M01008"; MonitorDevice = "P00120"; PlcId = "PLC1" }
          { Item.create Switch with Device = "M01009"; PlcId = "PLC2" }
          { Item.create NumDisplay with Device = "D100"; PlcId = "PLC2" }
          // PLC 를 고르지 않은 요소는 첫 번째 PLC 를 쓴다.
          { Item.create Lamp with Device = "P00121"; PlcId = "" } ]

    let plan = Project.scanAddressesByPlc plcs items
    let pick (id: string) =
        plan |> List.pick (fun (plcId, bits, words) -> if plcId = id then Some(bits, words) else None)
    let bits1, words1 = pick "PLC1"
    let bits2, words2 = pick "PLC2"

    Assert.Equal<string list>([ "M01008"; "P00120"; "P00121" ], bits1)
    Assert.Empty words1
    Assert.Equal<string list>([ "M01009" ], bits2)
    Assert.Equal<string list>([ "D100" ], words2)

[<Fact>]
let ``사용 안 하는 PLC 는 폴링하지 않는다`` () =
    let plcs = [ PlcLink.ethernet "PLC1"; { PlcLink.ethernet "PLC2" with Enabled = false } ]
    let items = [ { Item.create Switch with Device = "M01009"; PlcId = "PLC2" } ]
    let plan = Project.scanAddressesByPlc plcs items
    Assert.Single plan |> ignore
    Assert.Equal("PLC1", (let (id, _, _) = plan.Head in id))

// ---------------------------------------------------------------------------
//  PlcService — 여러 대를 한꺼번에
// ---------------------------------------------------------------------------

let private link (id: string) (port: int) (name: string) =
    { PlcLink.ethernet id with Name = name; Ip = "127.0.0.1"; Port = port; CycleMs = 100 }

[<Fact>]
let ``PLC 두 대를 함께 붙이고 값을 따로 본다`` () =
    use first = new FakeXgtServer()
    use second = new FakeXgtServer()
    first.SetWord("%MW100", 0x0100us) // 1호기 M01008 ON
    second.SetWord("%MW100", 0x0000us) // 2호기 OFF
    second.SetWord("%DW100", 4242us)

    use service = new PlcService()
    service.SetScanPlan(fun () ->
        [ { PlcId = "PLC1"; Bits = [ "M01008" ]; Words = [] }
          { PlcId = "PLC2"; Bits = [ "M01008" ]; Words = [ "D100" ] } ])

    match service.Connect [ link "PLC1" first.Port "1호기"; link "PLC2" second.Port "2호기" ] with
    | Error m -> failwith m
    | Ok summary -> Assert.Contains("PLC2", summary)

    Assert.True(
        waitFor 3000 (fun () ->
            service.TryBit("PLC1", "M01008") = Some true
            && service.TryBit("PLC2", "M01008") = Some false
            && service.TryWord("PLC2", "D100") = Some 4242us),
        "두 회선의 값이 모두 들어오지 않았다")

    // 같은 주소라도 PLC 별로 값이 섞이지 않는다.
    Assert.NotEqual(service.TryBit("PLC1", "M01008"), service.TryBit("PLC2", "M01008"))

    // 쓰기도 고른 PLC 에만 간다.
    match service.WriteBitVerified("PLC2", "M01008", true) with
    | Ok(Some readback) -> Assert.True readback
    | Ok None -> failwith "읽기 확인 결과가 없다"
    | Error m -> failwith m

    Assert.Equal(Some 0x0100us, second.GetWord "%MW100")
    Assert.Equal(Some 0x0100us, first.GetWord "%MW100")
    Assert.Equal(2, service.LinkStatus.Length)
    service.Disconnect()

[<Fact>]
let ``한 대가 안 붙어도 나머지로 운전한다`` () =
    use alive = new FakeXgtServer()
    use service = new PlcService()
    let logs = ConcurrentQueue<LogLevel * string>()
    service.Log.Add(fun entry -> logs.Enqueue entry)
    service.SetScanPlan(fun () -> [ { PlcId = "PLC1"; Bits = [ "M01008" ]; Words = [] } ])

    // 두 번째는 아무도 듣지 않는 포트라 연결할 수 없다.
    let dead = { link "PLC2" 1 "죽은 회선" with Port = 65500 }
    match service.Connect [ link "PLC1" alive.Port "1호기"; dead ] with
    | Error m -> failwith ("한 대는 붙어야 한다: " + m)
    | Ok _ -> ()

    Assert.True(waitFor 3000 (fun () -> service.TryBit("PLC1", "M01008") = Some false), "살아 있는 회선이 돌지 않았다")
    Assert.Single service.LinkStatus |> ignore
    Assert.True(logs |> Seq.exists (fun (_, m) -> m.Contains "[PLC2]" && m.Contains "ERROR"))
    service.Disconnect()

[<Fact>]
let ``전부 못 붙으면 오류로 알려 준다`` () =
    use service = new PlcService()
    let result = service.Connect [ { link "PLC1" 65501 "죽은 회선" with Port = 65501 } ]
    Assert.True(match result with Error _ -> true | Ok _ -> false)
    Assert.Empty service.LinkStatus

[<Fact>]
let ``사용으로 둔 PLC 가 없으면 연결하지 않는다`` () =
    use service = new PlcService()
    let result = service.Connect [ { PlcLink.ethernet "PLC1" with Enabled = false } ]
    Assert.True(match result with Error _ -> true | Ok _ -> false)

[<Fact>]
let ``같은 회선에 국번이 겹치면 회선을 열기도 전에 막는다`` () =
    use service = new PlcService()
    let plcs =
        [ { PlcLink.serial "PLC1" LinkRs485 1 with SerialPort = "COM_NOT_REAL" }
          { PlcLink.serial "PLC2" LinkRs485 1 with SerialPort = "COM_NOT_REAL" } ]
    // 없는 포트를 열려고 하지도 않고 설정 오류로 먼저 막아야 한다.
    match service.Connect plcs with
    | Error m -> Assert.Contains("국번", m)
    | Ok _ -> failwith "국번이 겹쳤는데 연결을 시작했다"
    Assert.Empty service.LinkStatus

[<Fact>]
let ``PLC 를 고르지 않은 주소는 첫 번째 PLC 에서 찾는다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x0100us)
    use service = new PlcService()
    service.SetScanPlan(fun () -> [ { PlcId = "PLC1"; Bits = [ "M01008" ]; Words = [] } ])
    match service.Connect [ link "PLC1" server.Port "1호기" ] with
    | Error m -> failwith m
    | Ok _ -> ()

    Assert.True(waitFor 3000 (fun () -> service.TryBit "M01008" = Some true), "기본 PLC 로 값을 찾지 못했다")
    Assert.Equal("PLC1", service.DefaultPlcId)
    Assert.Equal(Some true, service.TryBit("", "M01008"))
    service.Disconnect()
