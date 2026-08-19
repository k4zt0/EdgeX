module ShotHarness.DragProbe

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Headless
open Avalonia.Input
open Avalonia.Threading
open Avalonia.VisualTree

/// 시각 트리에서 조건에 맞는 컨트롤을 모두 찾는다.
let rec descendants (v: Visual) : Visual seq =
    seq {
        for child in v.GetVisualChildren() do
            yield child
            yield! descendants child
    }

let cardBorders (window: Window) =
    descendants window
    |> Seq.choose (fun v ->
        match v with
        | :? Border as b when (b.Tag :? string) && not (isNull (b.Parent)) && (b.Parent :? Canvas) -> Some b
        | _ -> None)
    |> List.ofSeq

let toggleButtons (window: Window) =
    descendants window
    |> Seq.choose (fun v ->
        match v with
        | :? Primitives.ToggleButton as t -> Some t
        | _ -> None)
    |> List.ofSeq

let describe (b: Border) =
    sprintf "left=%.0f top=%.0f w=%.0f h=%.0f" (Canvas.GetLeft b) (Canvas.GetTop b) b.Bounds.Width b.Bounds.Height

/// 카드 한 장을 드래그해서 실제로 좌표가 바뀌는지 확인한다.
let run (window: Window) =
    Dispatcher.UIThread.RunJobs()

    let cards = cardBorders window
    printfn "카드 수: %d" cards.Length
    if cards.IsEmpty then failwith "캔버스에 카드가 없다"

    let card = cards.Head
    printfn "드래그 전: %s" (describe card)

    // 배치 편집 켜기 (F2)
    window.KeyPressQwerty(PhysicalKey.F2, RawInputModifiers.None)
    Dispatcher.UIThread.RunJobs()

    let toggles = toggleButtons window
    printfn "토글 버튼 상태: %A" (toggles |> List.map (fun t -> string t.Content, t.IsChecked))

    // 배치 편집 후에는 화면을 다시 만들므로 카드를 다시 찾는다.
    let card = (cardBorders window).Head
    printfn "배치 편집 ON 후: %s" (describe card)

    let translated = card.TranslatePoint(Point(card.Bounds.Width / 2.0, 12.0), window)
    if not translated.HasValue then failwith "카드 좌표를 창 좌표로 바꾸지 못했다"
    let center = translated.Value
    printfn "누를 위치(창 좌표): %.0f, %.0f" center.X center.Y

    window.MouseDown(center, MouseButton.Left)
    Dispatcher.UIThread.RunJobs()
    for step in 1..6 do
        window.MouseMove(Point(center.X + float step * 10.0, center.Y + float step * 5.0))
        Dispatcher.UIThread.RunJobs()
    window.MouseUp(Point(center.X + 60.0, center.Y + 30.0), MouseButton.Left)
    Dispatcher.UIThread.RunJobs()

    let after = (cardBorders window).Head
    printfn "드래그 후: %s" (describe after)

/// 계기(다이얼)를 눌러 값이 바뀌는지 확인한다.
/// '부품 편집' 을 끈 뒤 다이얼 위를 눌러 보고, 표시 숫자가 달라지는지 본다.
let probeGauge (window: Window) (gaugeId: string) =
    Dispatcher.UIThread.RunJobs()

    // 문서 탭 중 마지막(HMI)을 고른다.
    descendants window
    |> Seq.tryPick (fun v ->
        match v with
        | :? TabControl as t when t.ItemCount >= 3 -> Some t
        | _ -> None)
    |> Option.iter (fun tabs -> tabs.SelectedIndex <- tabs.ItemCount - 1)
    Dispatcher.UIThread.RunJobs()

    // '부품 편집' 토글을 끈다. 켜져 있으면 누르기가 배치 편집으로 먹힌다.
    let editToggle =
        toggleButtons window
        |> List.tryFind (fun t ->
            match t.Content with
            | :? string as s -> s = XgbHmi.Core.I18n.t "hmi.edit"
            | _ -> false)
    match editToggle with
    | Some t ->
        printfn "부품 편집 (누르기 전) = %A" t.IsChecked
        t.IsChecked <- System.Nullable false
        Dispatcher.UIThread.RunJobs()
        printfn "부품 편집 (끈 뒤)     = %A" t.IsChecked
    | None -> printfn "!! '부품 편집' 토글을 찾지 못했다"

    let gauge =
        descendants window
        |> Seq.tryPick (fun v ->
            match v with
            | :? Border as b when (b.Tag :? string) && (b.Tag :?> string) = gaugeId -> Some b
            | _ -> None)

    match gauge with
    | None -> printfn "!! 계기 부품을 화면에서 찾지 못했다 (id=%s)" gaugeId
    | Some g ->
        let textOf () =
            descendants g
            |> Seq.choose (fun v ->
                match v with
                | :? TextBlock as t when not (System.String.IsNullOrWhiteSpace t.Text) -> Some t.Text
                | _ -> None)
            |> List.ofSeq
        printfn "계기 크기 = %.0f x %.0f" g.Bounds.Width g.Bounds.Height
        printfn "누르기 전 표시 = %A" (textOf ())

        // 다이얼 오른쪽 위(값이 큰 쪽)를 누른다.
        let local = Point(g.Bounds.Width * 0.85, g.Bounds.Height * 0.35)
        match g.TranslatePoint(local, window) with
        | v when v.HasValue ->
            printfn "누를 위치(창 좌표) = %.0f, %.0f" v.Value.X v.Value.Y
            window.MouseDown(v.Value, MouseButton.Left)
            Dispatcher.UIThread.RunJobs()
            printfn "누른 직후 표시   = %A" (textOf ())
            window.MouseUp(v.Value, MouseButton.Left)
            Dispatcher.UIThread.RunJobs()
            printfn "뗀 뒤 표시       = %A" (textOf ())
        | _ -> printfn "!! 계기 좌표를 창 좌표로 바꾸지 못했다"
