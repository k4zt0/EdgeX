module XgbHmi.Tests.PlcServiceTests

open System
open System.Collections.Concurrent
open System.Threading
open Xunit
open XgbHmi.App.Services
open XgbHmi.Tests

/// 조건을 만족할 때까지 기다린다(최대 timeoutMs).
let private waitFor (timeoutMs: int) (check: unit -> bool) =
    let sw = Diagnostics.Stopwatch.StartNew()
    let mutable ok = check ()
    while not ok && sw.ElapsedMilliseconds < int64 timeoutMs do
        Thread.Sleep 25
        ok <- check ()
    ok

let private start (server: FakeXgtServer) (trace: bool) =
    let service = new PlcService()
    let logs = ConcurrentQueue<LogLevel * string>()
    service.Log.Add(fun entry -> logs.Enqueue entry)
    service.TraceEnabled <- trace
    service.SetScanProvider(fun () -> [ "M01008" ], [ "D100" ])
    let result = service.Connect("127.0.0.1", server.Port, 100)
    Assert.True((match result with Ok _ -> true | Error _ -> false))
    service, logs

let private texts (logs: ConcurrentQueue<LogLevel * string>) =
    logs |> Seq.map snd |> List.ofSeq

[<Fact>]
let ``값이 바뀌면 출력 창에 변화가 기록된다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x0000us)
    let service, logs = start server false
    use service = service

    Assert.True(waitFor 3000 (fun () -> service.TryBit "M01008" = Some false), "첫 스캔이 오지 않았다")

    // PLC 쪽에서 M01008(=MW100 bit8) 이 켜졌다고 가정한다.
    server.SetWord("%MW100", 0x0100us)

    Assert.True(
        waitFor 3000 (fun () -> texts logs |> List.exists (fun t -> t.Contains "CHANGE M01008 : OFF -> ON")),
        sprintf "변화 기록이 없다: %A" (texts logs))

    // WORD 값 변화도 남는다.
    server.SetWord("%DW100", 1234us)
    Assert.True(
        waitFor 3000 (fun () -> texts logs |> List.exists (fun t -> t.Contains "CHANGE D100" && t.Contains "-> 1234")),
        sprintf "WORD 변화 기록이 없다: %A" (texts logs))

    service.Disconnect()

[<Fact>]
let ``변화 기록을 끄면 남기지 않는다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x0000us)
    let service, logs = start server false
    use service = service
    service.LogChanges <- false

    Assert.True(waitFor 3000 (fun () -> service.TryBit "M01008" = Some false))
    server.SetWord("%MW100", 0x0100us)
    Assert.True(waitFor 3000 (fun () -> service.TryBit "M01008" = Some true), "값이 갱신되지 않았다")

    Assert.DoesNotContain(texts logs, fun t -> t.Contains "CHANGE M01008")
    service.Disconnect()

[<Fact>]
let ``추적을 켜면 스캔 요약과 TX RX 원문이 출력 창에 남는다`` () =
    use server = new FakeXgtServer()
    let service, logs = start server true
    use service = service

    Assert.True(
        waitFor 3000 (fun () -> texts logs |> List.exists (fun t -> t.StartsWith "TX" && t.Contains "%MW100")),
        sprintf "TX 기록이 없다: %A" (texts logs))
    Assert.True(
        waitFor 3000 (fun () -> texts logs |> List.exists (fun t -> t.StartsWith "RX" && t.Contains "cmd=0x0055")),
        "RX 기록이 없다")
    Assert.True(
        waitFor 3000 (fun () -> texts logs |> List.exists (fun t -> t.Contains "SCAN #" && t.Contains "비트 1개")),
        sprintf "스캔 요약이 없다: %A" (texts logs))

    // 통계도 함께 올라간다.
    Assert.True(service.CycleCount > 0L)
    Assert.True(service.FrameCount > 0L)
    Assert.Equal(0L, service.ErrorCount)
    Assert.True(service.LastCycleMs >= 0.0)

    service.Disconnect()

[<Fact>]
let ``쓰기는 검증 결과와 함께 기록된다`` () =
    use server = new FakeXgtServer()
    server.SetWord("%MW100", 0x00FFus)
    let service, logs = start server false
    use service = service

    match service.WriteBitVerified("M01008", true) with
    | Ok(Some readback) -> Assert.True readback
    | Ok None -> failwith "읽기 확인 결과가 없다"
    | Error m -> failwith m

    // 다른 비트는 그대로여야 한다.
    Assert.Equal(Some 0x01FFus, server.GetWord "%MW100")
    Assert.True(texts logs |> List.exists (fun t -> t.Contains "WRITE BIT M01008 = ON" && t.Contains "완료"))

    service.Disconnect()
    Assert.True(texts logs |> List.exists (fun t -> t.Contains "DISCONNECT 요청" && t.Contains "프레임"))
