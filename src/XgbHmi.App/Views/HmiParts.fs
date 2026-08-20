/// 터치스크린(HMI) 부품 한 개를 화면에 그린다.
/// 부품은 제 주소를 갖지 않는다. 연결한 화면 요소(ElementVm)의 주소로 읽고 쓴다.
module XgbHmi.App.Views.HmiParts

open System
open Avalonia
open Avalonia.Controls
open Avalonia.Controls.Primitives
open Avalonia.Input
open Avalonia.Layout
open Avalonia.Media
open XgbHmi.Core
open XgbHmi.Protocol
open XgbHmi.App.Themes
open XgbHmi.App.ViewModels

/// 버튼 한 번 누름. 같은 코일에 대한 쓰기 순서가 뒤엉키지 않도록 한 묶음으로 넘긴다.
/// (그룹 해제 -> 본 동작 -> 후속 상태 순서로 실행된다)
type PressPlan =
    { Target: ElementVm
      Action: SwitchAction
      /// 같은 상호 배타 그룹의 다른 코일. 본 동작 전에 OFF 로 쓴다.
      ResetOff: ElementVm list
      /// 본 동작 뒤에 ON 으로 만들 요소.
      ThenOn: ElementVm option }

/// 부품이 바깥(메인 창)에 요청하는 것들
type PartHost =
    { /// PLC 조작 통로. 운전 화면 카드와 같은 것을 쓴다.
      Cards: CardFactory.CardCallbacks
      /// 부품이 연결한 화면 요소를 Id 로 찾는다.
      Resolve: string -> ElementVm option
      /// 부품 편집 중이면 true. 이때는 눌러도 PLC로 명령이 나가지 않는다.
      Editing: unit -> bool
      /// 같은 상호 배타 그룹에 있는 다른 부품들이 가리키는 요소. (자기 자신은 빼고)
      GroupPeers: string -> string -> ElementVm list
      /// 그 요소들을 한꺼번에 OFF 로 쓴다. 쓰기가 잠겨 있으면 조용히 아무것도 하지 않는다.
      ResetGroup: ElementVm list -> unit
      /// 비트 버튼 한 번 누름을 순서대로 실행한다.
      Press: ElementVm -> SwitchAction -> HmiPartVm -> unit }

/// 캔버스에 올라간 부품 하나
type PartVisual =
    { Vm: HmiPartVm
      Root: Border
      /// 최신 PLC 값으로 표시를 갱신한다.
      Refresh: CardFactory.RuntimeStatus -> unit
      /// 크기가 바뀌었을 때 모서리 둥글기 등을 다시 맞춘다.
      Resize: unit -> unit }

// ---------------------------------------------------------------------------
//  색 / 글꼴 도우미
// ---------------------------------------------------------------------------

let private orElse (fallback: string) (hex: string) =
    if String.IsNullOrWhiteSpace hex then fallback else hex

/// 점등 발광. 테마의 Glow 설정과 무관하게 점등은 항상 보여야 한다.
let private glow (hex: string) (blur: float) =
    let c = Color.Parse hex
    BoxShadows.Parse(sprintf "0 0 %g 0 %s" blur ((Color.FromArgb(150uy, c.R, c.G, c.B)).ToString()))

/// 밝은 바탕 위에는 검은 글자, 어두운 바탕 위에는 흰 글자.
let private textOn (hex: string) =
    let c = Color.Parse hex
    let luma = (0.299 * float c.R + 0.587 * float c.G + 0.114 * float c.B) / 255.0
    if luma > 0.62 then "#101318" else "#FFFFFF"

/// 두 색을 섞는다. t=0 이면 a, t=1 이면 b.
let private mix (a: string) (b: string) (t: float) =
    let ca = Color.Parse a
    let cb = Color.Parse b
    let f (x: byte) (y: byte) = byte (float x * (1.0 - t) + float y * t)
    (Color.FromRgb(f ca.R cb.R, f ca.G cb.G, f ca.B cb.B)).ToString()

/// 패널 바탕색. 비워 두면 실제 터치패널처럼 어두운 바탕을 쓴다.
/// (부품 색은 에디터 테마가 아니라 이 바탕에서 끌어와야 어떤 테마에서도 글자가 읽힌다)
let resolveBackground (p: Palette) (background: string) =
    if String.IsNullOrWhiteSpace background then
        (if p.IsDark then p.CanvasBg else "#1A1E25")
    else background

/// 색을 어둡게 (셀렉터 트랙, 눌림 표시 등)
let private darken (hex: string) (amount: float) =
    let c = Color.Parse hex
    let f v = byte (max 0.0 (float v * (1.0 - amount)))
    (Color.FromRgb(f c.R, f c.G, f c.B)).ToString()

let private textAlignOf (align: string) =
    match align with
    | "LEFT" -> TextAlignment.Left
    | "RIGHT" -> TextAlignment.Right
    | _ -> TextAlignment.Center

let private hAlignOf (align: string) =
    match align with
    | "LEFT" -> HorizontalAlignment.Left
    | "RIGHT" -> HorizontalAlignment.Right
    | _ -> HorizontalAlignment.Center

/// 부품 모양에 맞는 모서리 둥글기
let private cornerOf (vm: HmiPartVm) =
    if vm.Shape = HmiShape.circle then
        CornerRadius(min (float vm.Width) (float vm.Height) / 2.0)
    elif vm.Shape = HmiShape.round then
        CornerRadius(float vm.Height / 2.0)
    else
        CornerRadius(float vm.Corner)

/// 부품에 쓸 글자. 비어 있으면 연결한 요소의 이름을 쓴다.
let private captionOf (host: PartHost) (vm: HmiPartVm) =
    if not (String.IsNullOrWhiteSpace vm.Text) then vm.Text
    else
        match host.Resolve vm.TargetId with
        | Some t when not (String.IsNullOrWhiteSpace t.Name) -> t.Name
        | Some t -> t.Device
        | None -> ""

/// 이 부품이 수행할 스위치 동작. 부품에서 고른 것이 있으면 그것을, 없으면 요소 설정을 쓴다.
/// 같은 코일에 '운전'(ON)과 '정지'(OFF) 버튼을 따로 두는 데 쓴다.
let private effectiveAction (vm: HmiPartVm) (target: ElementVm) =
    if String.IsNullOrWhiteSpace vm.Action then target.Action else SwitchAction.parse vm.Action

/// PLC 의 정수값을 부품 설정대로 글자로 만든다. 소수점 자리가 1이면 80 -> "8.0".
let private formatValue (vm: HmiPartVm) (value: int) =
    let text =
        if vm.Decimals <= 0 then string value
        else
            let scale = pown 10 vm.Decimals
            let sign = if value < 0 then "-" else ""
            let a = abs value
            sprintf "%s%d.%0*d" sign (a / scale) vm.Decimals (a % scale)
    if String.IsNullOrWhiteSpace vm.Unit then text else text + " " + vm.Unit

/// 계기·막대가 쓸 눈금 범위. 부품에 따로 넣은 값이 있으면 그것을, 없으면 요소의 최소~최대.
/// 요소 범위는 PLC 데이터 범위(D200 이면 -32768~65535)라 그대로 쓰면 다이얼이 너무 거칠다.
let private scaleOf (vm: HmiPartVm) (t: ElementVm) =
    if vm.ScaleMax > vm.ScaleMin then vm.ScaleMin, vm.ScaleMax else t.Min, t.Max

/// 연결한 요소의 WORD 값을 부호까지 살펴 정수로 읽는다.
let private wordValue (status: CardFactory.RuntimeStatus) (t: ElementVm) =
    match status.WordOf t.PlcId t.Device with
    | Some raw -> Some(if t.Min < 0 then int (int16 raw) else int raw)
    | None -> None

/// 스위치가 실제로 돌아온 상태. 상태확인 디바이스가 있으면 그쪽을 먼저 본다.
/// (운전 화면 카드의 liveBit 과 같은 규칙)
let private liveBit (status: CardFactory.RuntimeStatus) (t: ElementVm) =
    if String.IsNullOrWhiteSpace t.MonitorDevice then status.BitOf t.PlcId t.Device
    else
        match status.BitOf t.PlcId t.MonitorDevice with
        | Some v -> Some v
        | None -> status.BitOf t.PlcId t.Device

// ---------------------------------------------------------------------------
//  아날로그 계기 (바늘이 도는 원형 미터)
// ---------------------------------------------------------------------------

type internal GaugeFace() =
    inherit Control()

    /// 눈금이 도는 범위. 시작 각도와 벌어진 각도. (Render 와 좌표 계산이 함께 쓴다)
    static member val StartDegrees = 150.0
    static member val SweepDegrees = 240.0

    /// 0.0 ~ 1.0. 값을 모르면 None.
    member val Ratio: float option = None with get, set
    member val TrackColor = "#2A2F38" with get, set
    member val ArcColor = "#E0457B" with get, set
    member val NeedleColor = "#F2F4F8" with get, set

    override this.Render(ctx: DrawingContext) =
        let w = this.Bounds.Width
        let h = this.Bounds.Height
        if w > 4.0 && h > 4.0 then
            let cx = w / 2.0
            let cy = h * 0.56
            let r = (min w h) * 0.40
            if r > 2.0 then
                let startDeg = GaugeFace.StartDegrees
                let sweep = GaugeFace.SweepDegrees
                let pointAt (deg: float) (radius: float) =
                    let rad = deg * Math.PI / 180.0
                    Point(cx + radius * cos rad, cy + radius * sin rad)

                // 도형을 다 그린 뒤 컨텍스트를 닫아야 완성된 경로가 나온다.
                let arcGeometry (fromDeg: float) (toDeg: float) =
                    let g = StreamGeometry()
                    let gctx = g.Open()
                    gctx.BeginFigure(pointAt fromDeg r, false)
                    gctx.ArcTo(pointAt toDeg r, Size(r, r), 0.0, (toDeg - fromDeg) > 180.0, SweepDirection.Clockwise)
                    gctx.EndFigure false
                    gctx.Dispose()
                    g

                let arc (fromDeg: float) (toDeg: float) (brush: IBrush) (thickness: float) =
                    if toDeg - fromDeg > 0.05 then
                        ctx.DrawGeometry(null, Pen(brush, thickness, lineCap = PenLineCap.Round), arcGeometry fromDeg toDeg)

                let thickness = max 6.0 (r * 0.20)
                arc startDeg (startDeg + sweep) (Ui.brush this.TrackColor) thickness

                match this.Ratio with
                | Some ratio ->
                    let ratio = max 0.0 (min 1.0 ratio)
                    arc startDeg (startDeg + sweep * ratio) (Ui.brush this.ArcColor) thickness
                    // 바늘
                    let tip = pointAt (startDeg + sweep * ratio) (r * 0.86)
                    let tail = pointAt (startDeg + sweep * ratio + 180.0) (r * 0.16)
                    ctx.DrawLine(Pen(Ui.brush this.NeedleColor, max 2.0 (r * 0.055), lineCap = PenLineCap.Round), tail, tip)
                    ctx.DrawEllipse(Ui.brush this.NeedleColor, null, Point(cx, cy), r * 0.10, r * 0.10)
                | None ->
                    ctx.DrawEllipse(Ui.brush this.TrackColor, null, Point(cx, cy), r * 0.10, r * 0.10)

/// 눈금판 위의 한 점이 가리키는 비율(0~1). 손가락으로 바늘을 돌릴 때 쓴다.
/// 눈금이 없는 아래쪽을 가리키면 가까운 끝으로 붙인다.
let private ratioAtPoint (size: Size) (point: Point) =
    let cx = size.Width / 2.0
    let cy = size.Height * 0.56
    let dx = point.X - cx
    let dy = point.Y - cy
    if abs dx < 0.5 && abs dy < 0.5 then None
    else
        let degrees =
            let raw = Math.Atan2(dy, dx) * 180.0 / Math.PI
            // 시작 각도(150도)를 0 으로 두고 시계 방향으로 편다.
            let shifted = raw - GaugeFace.StartDegrees
            let wrapped = ((shifted % 360.0) + 360.0) % 360.0
            wrapped
        if degrees <= GaugeFace.SweepDegrees then Some(degrees / GaugeFace.SweepDegrees)
        else
            // 눈금 밖(아래쪽 빈 구간). 어느 끝에 더 가까운지로 0 또는 1 로 붙인다.
            let past = degrees - GaugeFace.SweepDegrees
            let gap = 360.0 - GaugeFace.SweepDegrees
            Some(if past > gap / 2.0 then 0.0 else 1.0)

// ---------------------------------------------------------------------------
//  로터리 셀렉터 눈금판
// ---------------------------------------------------------------------------

type internal RotaryFace() =
    inherit Control()

    /// 지금 자리 (0 부터). 값을 모르면 None.
    member val Position: int option = None with get, set
    member val Count = 2 with get, set
    member val DialColor = "#2A2F38" with get, set
    member val KnobColor = "#C8CDD6" with get, set
    member val MarkColor = "#F2F4F8" with get, set
    member val TickColor = "#6B7480" with get, set

    override this.Render(ctx: DrawingContext) =
        let w = this.Bounds.Width
        let h = this.Bounds.Height
        if w > 8.0 && h > 8.0 then
            let cx = w / 2.0
            let cy = h / 2.0
            let r = (min w h) * 0.42
            let count = max 2 this.Count
            // 자리들은 위쪽 240도 범위에 고르게 놓는다. (150도 -> 390도)
            let angleOf (i: int) = 150.0 + 240.0 * float i / float (count - 1)
            let pointAt (deg: float) (radius: float) =
                let rad = deg * Math.PI / 180.0
                Point(cx + radius * cos rad, cy + radius * sin rad)

            // 자리 눈금
            for i in 0 .. count - 1 do
                let a = angleOf i
                let lit = this.Position = Some i
                let pen = Pen(Ui.brush (if lit then this.MarkColor else this.TickColor), (if lit then 3.0 else 2.0), lineCap = PenLineCap.Round)
                ctx.DrawLine(pen, pointAt a (r * 1.02), pointAt a (r * 1.24))

            // 손잡이
            ctx.DrawEllipse(Ui.brush this.DialColor, Pen(Ui.brush this.TickColor, 1.5), Point(cx, cy), r, r)
            ctx.DrawEllipse(Ui.brush this.KnobColor, null, Point(cx, cy), r * 0.82, r * 0.82)

            // 손잡이 위의 지시선
            match this.Position with
            | Some i ->
                let a = angleOf (max 0 (min (count - 1) i))
                ctx.DrawLine(
                    Pen(Ui.brush "#22262E", max 3.0 (r * 0.13), lineCap = PenLineCap.Round),
                    Point(cx, cy),
                    pointAt a (r * 0.72))
            | None -> ()

// ---------------------------------------------------------------------------
//  숫자 키패드 (값 부품을 눌렀을 때)
// ---------------------------------------------------------------------------

/// 터치로 값을 넣는 키패드를 부품 옆에 띄운다.
let private showKeypad (p: Palette) (anchor: Control) (target: ElementVm) (current: int option) (write: int -> unit) =
    let flyout = Flyout(Placement = PlacementMode.Bottom, ShowMode = FlyoutShowMode.Standard)

    let entry = Ui.mono 30.0 (match current with Some v -> string v | None -> "0")
    entry.HorizontalAlignment <- HorizontalAlignment.Right
    entry.Foreground <- Ui.brush p.Text
    let mutable buffer = ""
    let mutable negative = false

    let title = Ui.muted (I18n.t "hmi.keypad" + "   " + target.Device)
    title.FontSize <- 11.5

    let range = Ui.muted (sprintf "%d ~ %d" target.Min target.Max)
    range.FontSize <- 11.0

    let render () =
        let shown = if buffer = "" then "0" else buffer
        entry.Text <- (if negative then "-" else "") + shown

    let display =
        Border(
            Background = Ui.brush p.SurfaceAlt,
            BorderBrush = Ui.brush p.Border,
            BorderThickness = Thickness 1.0,
            CornerRadius = CornerRadius 6.0,
            Padding = Thickness(12.0, 6.0, 12.0, 6.0),
            Child = entry
        )

    let grid = Grid(RowSpacing = 6.0, ColumnSpacing = 6.0)
    for _ in 1..4 do
        grid.RowDefinitions.Add(RowDefinition(GridLength(64.0, GridUnitType.Pixel)))
    for _ in 1..3 do
        grid.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))

    let key (caption: string) (row: int) (col: int) (accent: string option) (onClick: unit -> unit) =
        let text = Ui.title 20.0 caption
        text.HorizontalAlignment <- HorizontalAlignment.Center
        let fill = accent |> Option.defaultValue p.SurfaceAlt
        let b =
            Button(
                Content = text,
                MinWidth = 62.0,
                Background = Ui.brush fill,
                Foreground = Ui.brush (match accent with Some a -> textOn a | None -> p.Text),
                BorderBrush = Ui.brush p.Border,
                BorderThickness = Thickness 1.0,
                CornerRadius = CornerRadius 8.0,
                HorizontalContentAlignment = HorizontalAlignment.Center,
                VerticalContentAlignment = VerticalAlignment.Center,
                FontFamily = Ui.uiFont
            )
        b.Click.Add(fun _ -> onClick ())
        Grid.SetRow(b, row)
        Grid.SetColumn(b, col)
        grid.Children.Add b

    let digit (d: int) (row: int) (col: int) =
        key (string d) row col None (fun () ->
            if buffer.Length < 6 then
                buffer <- (if buffer = "0" then "" else buffer) + string d
                render ())

    digit 7 0 0
    digit 8 0 1
    digit 9 0 2
    digit 4 1 0
    digit 5 1 1
    digit 6 1 2
    digit 1 2 0
    digit 2 2 1
    digit 3 2 2
    key "±" 3 0 None (fun () ->
        negative <- not negative
        render ())
    digit 0 3 1
    key "⌫" 3 2 None (fun () ->
        buffer <- (if buffer.Length > 0 then buffer.Substring(0, buffer.Length - 1) else "")
        render ())

    let clear =
        Ui.button "CLR" [] (fun () ->
            buffer <- ""
            negative <- false
            render ())
    clear.MinWidth <- 84.0

    let enter =
        Ui.button "ENT" [ "primary" ] (fun () ->
            let text = (if negative then "-" else "") + (if buffer = "" then "0" else buffer)
            match Int32.TryParse text with
            | true, v ->
                // 요소에 정해 둔 범위를 벗어나면 그 안으로 잘라서 넣는다. (오조작 방지)
                write (max target.Min (min target.Max v))
                flyout.Hide()
            | _ -> ())
    enter.MinWidth <- 106.0

    let actions = Ui.stackH 6.0 [ clear; enter ]
    actions.HorizontalAlignment <- HorizontalAlignment.Right

    let root = Ui.stackV 8.0 [ title; display; range; grid; actions ]
    root.Width <- 240.0

    flyout.Content <- Border(Padding = Thickness 10.0, Child = root)
    flyout.ShowAt anchor

// ---------------------------------------------------------------------------
//  부품 만들기
// ---------------------------------------------------------------------------

/// 부품 한 개를 만든다. 돌려주는 Root 는 캔버스에 절대 좌표로 놓인다.
/// background 는 패널 바탕색(이미 resolveBackground 를 거친 값)이다.
let create (p: Palette) (background: string) (vm: HmiPartVm) (host: PartHost) : PartVisual =

    let root =
        Border(
            Background = Brushes.Transparent,
            Width = float vm.Width,
            Height = float vm.Height,
            Tag = vm.Id,
            ClipToBounds = false
        )

    /// 지금 이 부품을 눌러도 되는지. 편집 중에는 PLC로 명령을 보내지 않는다.
    let touchable () = HmiPartKind.isTouchable vm.Kind && not (host.Editing ())

    /// 상호 배타 그룹이 걸려 있으면 같은 그룹의 다른 코일을 먼저 끈다.
    /// 실행 / 중지 / 종료처럼 한 번에 하나만 켜져야 하는 조작에 쓴다.
    let resetGroupPeers () =
        if not (String.IsNullOrWhiteSpace vm.Group) then
            host.ResetGroup(host.GroupPeers vm.Group vm.Id)

    let target () = host.Resolve vm.TargetId

    // 부품 기본색은 패널 바탕에서 끌어온다. 에디터 테마를 바꿔도 패널 위 글자는 그대로 읽힌다.
    let ink = textOn background
    let dim = mix ink background 0.45
    let face = mix background ink 0.12
    let edge = mix background ink 0.42

    // 상태 표시에 쓰는 색
    let defaultOn = orElse p.On vm.OnColor
    let defaultOff = orElse face vm.OffColor

    match vm.Kind with

    // ---------------- 버튼 / 램프 ----------------
    | PartButton
    | PartLamp ->
        let shell =
            Border(
                Background = Ui.brush defaultOff,
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 2.0,
                CornerRadius = cornerOf vm,
                ClipToBounds = true
            )

        let caption = Ui.title (float vm.FontSize) (captionOf host vm)
        caption.TextAlignment <- TextAlignment.Center
        caption.TextWrapping <- TextWrapping.Wrap
        caption.HorizontalAlignment <- HorizontalAlignment.Center
        caption.VerticalAlignment <- VerticalAlignment.Center
        caption.Margin <- Thickness 6.0
        caption.Foreground <- Ui.brush (orElse (textOn defaultOff) vm.TextColor)
        shell.Child <- caption
        root.Child <- shell

        // 눌린 동안 살짝 어둡게. 손끝 반응이 보여야 오조작이 준다.
        let mutable held = false
        let mutable lastOn = false

        let paint (isOn: bool) (fault: bool) =
            let fill =
                if fault then p.Error
                elif isOn then defaultOn
                else defaultOff
            let fill = if held then darken fill 0.22 else fill
            shell.Background <- Ui.brush fill
            shell.BoxShadow <- (if isOn && not fault then glow fill (float vm.Height * 0.28) else BoxShadows())
            caption.Foreground <- Ui.brush (orElse (textOn fill) vm.TextColor)
            let text =
                if isOn && not (String.IsNullOrWhiteSpace vm.OnText) then vm.OnText
                elif not isOn && not (String.IsNullOrWhiteSpace vm.OffText) then vm.OffText
                else captionOf host vm
            caption.Text <- text

        if vm.Kind = PartButton then
            root.PointerPressed.Add(fun e ->
                if touchable () && e.GetCurrentPoint(root).Properties.IsLeftButtonPressed then
                    held <- true
                    paint lastOn false
                    match target () with
                    | Some t when effectiveAction vm t = Momentary -> host.Cards.MomentaryDown t
                    | _ -> ())

            root.PointerReleased.Add(fun e ->
                if held then
                    held <- false
                    paint lastOn false
                    if touchable () then
                        match target () with
                        | Some t -> host.Press t (effectiveAction vm t) vm
                        | None -> ())

            // 손가락이 부품 밖으로 나가면 순간 스위치는 반드시 뗀다.
            root.PointerExited.Add(fun _ ->
                if held then
                    held <- false
                    paint lastOn false
                    match target () with
                    | Some t when effectiveAction vm t = Momentary -> host.Cards.MomentaryUp t
                    | _ -> ())

            root.Cursor <- new Cursor(StandardCursorType.Hand)

        let refresh (status: CardFactory.RuntimeStatus) =
            match target () with
            | None ->
                shell.Background <- Ui.tint p.Off 0.20
                shell.BorderBrush <- Ui.brush p.Warn
                caption.Text <- (if String.IsNullOrWhiteSpace vm.Text then I18n.t "hmi.needTarget" else vm.Text)
                caption.Foreground <- Ui.brush dim
            | Some t ->
                shell.BorderBrush <- Ui.brush (orElse edge vm.BorderColor)
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                let live = liveBit status t
                // OFF 버튼은 비트가 꺼져 있을 때가 '지금 상태' 다. (운전 화면 카드와 같은 규칙)
                lastOn <-
                    if vm.Kind = PartButton && effectiveAction vm t = Off then live = Some false
                    else live = Some true
                paint lastOn fault
                // 값을 아직 모를 때는 흐리게 둔다. 꺼짐과 구분해야 한다.
                // 다만 편집 중에는 작화가 또렷하게 보여야 하므로 흐리게 하지 않는다.
                shell.Opacity <- (if live.IsNone && not fault && not (host.Editing ()) then 0.55 else 1.0)

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> shell.CornerRadius <- cornerOf vm }

    // ---------------- 셀렉터 스위치 ----------------
    | PartToggle ->
        let layout = Grid()
        layout.RowDefinitions.Add(RowDefinition(GridLength(1.0, GridUnitType.Star)))
        layout.RowDefinitions.Add(RowDefinition(GridLength.Auto))

        let trackColor = orElse (mix background ink 0.06) vm.OffColor
        let track =
            Border(
                Background = Ui.brush trackColor,
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 2.0,
                Margin = Thickness(2.0, 2.0, 2.0, 2.0)
            )

        let knob =
            Border(
                Background = Ui.brush dim,
                CornerRadius = CornerRadius 999.0,
                HorizontalAlignment = HorizontalAlignment.Left,
                Margin = Thickness 5.0
            )

        let knobHost = Grid()
        knobHost.Children.Add track
        knobHost.Children.Add knob
        Grid.SetRow(knobHost, 0)
        layout.Children.Add knobHost

        let offLabel = Ui.text ""
        offLabel.FontSize <- float vm.FontSize
        offLabel.Foreground <- Ui.brush dim
        let nameLabel = Ui.title (float vm.FontSize) (captionOf host vm)
        nameLabel.Foreground <- Ui.brush (orElse ink vm.TextColor)
        nameLabel.TextTrimming <- TextTrimming.CharacterEllipsis
        let onLabel = Ui.text ""
        onLabel.FontSize <- float vm.FontSize
        onLabel.Foreground <- Ui.brush dim

        let labels = Grid(Margin = Thickness(4.0, 2.0, 4.0, 0.0))
        labels.ColumnDefinitions.Add(ColumnDefinition(GridLength.Auto))
        labels.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))
        labels.ColumnDefinitions.Add(ColumnDefinition(GridLength.Auto))
        nameLabel.HorizontalAlignment <- HorizontalAlignment.Center
        Grid.SetColumn(offLabel, 0)
        Grid.SetColumn(nameLabel, 1)
        Grid.SetColumn(onLabel, 2)
        labels.Children.Add offLabel
        labels.Children.Add nameLabel
        labels.Children.Add onLabel
        Grid.SetRow(labels, 1)
        layout.Children.Add labels

        root.Child <- layout

        if HmiPartKind.isTouchable vm.Kind then
            root.Cursor <- new Cursor(StandardCursorType.Hand)
            root.PointerReleased.Add(fun _ ->
                if touchable () then
                    match target () with
                    | Some t ->
                        // 셀렉터는 좌우 두 자리뿐이라 순간 동작도 토글로 다룬다.
                        let action =
                            match effectiveAction vm t with
                            | On -> On
                            | Off -> Off
                            | _ -> Toggle
                        host.Press t action vm
                    | None -> ())

        let applyKnob (isOn: bool) =
            knob.HorizontalAlignment <- (if isOn then HorizontalAlignment.Right else HorizontalAlignment.Left)
            let size = max 10.0 (float vm.Height * 0.52 - 10.0)
            knob.Width <- size
            knob.Height <- size
            track.CornerRadius <- CornerRadius(float vm.Height * 0.26)

        let refresh (status: CardFactory.RuntimeStatus) =
            offLabel.Text <- (if String.IsNullOrWhiteSpace vm.OffText then "OFF" else vm.OffText)
            onLabel.Text <- (if String.IsNullOrWhiteSpace vm.OnText then "ON" else vm.OnText)
            nameLabel.Text <- captionOf host vm
            match target () with
            | None ->
                applyKnob false
                knob.Background <- Ui.brush dim
                track.BorderBrush <- Ui.brush p.Warn
                nameLabel.Text <- I18n.t "hmi.needTarget"
            | Some t ->
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                let live = liveBit status t
                let isOn = live = Some true
                applyKnob isOn
                track.BorderBrush <- Ui.brush (if fault then p.Error else orElse edge vm.BorderColor)
                track.Background <- Ui.brush (if isOn then orElse p.On vm.OnColor else trackColor)
                knob.Background <- Ui.brush (if fault then p.Error else if isOn then textOn (orElse p.On vm.OnColor) else dim)
                offLabel.Foreground <- Ui.brush (if isOn then dim else ink)
                onLabel.Foreground <- Ui.brush (if isOn then ink else dim)
                root.Opacity <- (if live.IsNone && not fault && not (host.Editing ()) then 0.55 else 1.0)

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> applyKnob (knob.HorizontalAlignment = HorizontalAlignment.Right) }

    // ---------------- 숫자값 ----------------
    | PartValue ->
        let shell =
            Border(
                Background = Ui.brush (orElse face vm.OffColor),
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 2.0,
                CornerRadius = cornerOf vm,
                ClipToBounds = true,
                Padding = Thickness(10.0, 6.0, 10.0, 6.0)
            )

        let body = Grid(ColumnSpacing = 6.0)
        body.ColumnDefinitions.Add(ColumnDefinition(GridLength.Auto))
        body.ColumnDefinitions.Add(ColumnDefinition(GridLength(1.0, GridUnitType.Star)))

        // ▲▼ 미세 조정. 증감폭이 0 이면 두지 않는다.
        let stepper = Ui.stackV 3.0 []
        stepper.VerticalAlignment <- VerticalAlignment.Center
        Grid.SetColumn(stepper, 0)
        body.Children.Add stepper

        let valueText = Ui.mono (float vm.FontSize) "----"
        valueText.FontWeight <- FontWeight.Bold
        valueText.Foreground <- Ui.brush (orElse ink vm.TextColor)
        valueText.HorizontalAlignment <- HorizontalAlignment.Right

        let subText = Ui.mono (max 10.0 (float vm.FontSize * 0.45)) ""
        subText.Foreground <- Ui.brush dim
        subText.HorizontalAlignment <- HorizontalAlignment.Right

        let caption = Ui.text (captionOf host vm)
        caption.FontSize <- max 9.0 (float vm.FontSize * 0.34)
        caption.Foreground <- Ui.brush dim
        caption.HorizontalAlignment <- HorizontalAlignment.Right
        caption.TextTrimming <- TextTrimming.CharacterEllipsis

        let numbers = Ui.stackV 0.0 [ caption; valueText; subText ]
        numbers.VerticalAlignment <- VerticalAlignment.Center
        Grid.SetColumn(numbers, 1)
        body.Children.Add numbers
        shell.Child <- body
        root.Child <- shell

        /// 지금 값에 delta 를 더해 쓴다. 요소에 정해 둔 범위를 넘지 않는다.
        let mutable lastValue: int option = None

        let bump (delta: int) =
            if touchable () then
                match target () with
                | Some t when t.Kind = NumInput ->
                    let current = lastValue |> Option.defaultValue t.Min
                    let next = max t.Min (min t.Max (current + delta))
                    if next <> current then host.Cards.NumericWrite t next
                | _ -> ()

        let arrow (caption: string) (delta: unit -> int) =
            let text = Ui.title 13.0 caption
            text.HorizontalAlignment <- HorizontalAlignment.Center
            let b =
                Button(
                    Content = text,
                    Width = 34.0,
                    Height = 28.0,
                    Padding = Thickness 0.0,
                    Background = Ui.tint p.Accent 0.20,
                    BorderBrush = Ui.brush p.Border,
                    BorderThickness = Thickness 1.0,
                    CornerRadius = CornerRadius 5.0,
                    HorizontalContentAlignment = HorizontalAlignment.Center,
                    VerticalContentAlignment = VerticalAlignment.Center
                )
            b.Click.Add(fun _ -> bump (delta ()))
            b

        stepper.Children.Add(arrow "▲" (fun () -> vm.Step))
        stepper.Children.Add(arrow "▼" (fun () -> -vm.Step))
        // ▲▼ 를 누른 것은 여기서 끝낸다. 위로 흘려 보내면 키패드까지 함께 열린다.
        stepper.AddHandler(
            InputElement.PointerReleasedEvent,
            (fun _ (e: PointerReleasedEventArgs) -> e.Handled <- true),
            Interactivity.RoutingStrategies.Bubble
        )

        // 숫자를 누르면 키패드가 뜬다. 숫자입력 요소일 때만.
        root.PointerReleased.Add(fun _ ->
            if touchable () then
                match target () with
                | Some t when t.Kind = NumInput ->
                    showKeypad p (root :> Control) t lastValue (fun v -> host.Cards.NumericWrite t v)
                | _ -> ())

        let refresh (status: CardFactory.RuntimeStatus) =
            caption.Text <- captionOf host vm
            stepper.IsVisible <- (vm.Step > 0 && (match target () with Some t -> t.Kind = NumInput | None -> false))
            match target () with
            | None ->
                valueText.Text <- "----"
                subText.Text <- I18n.t "hmi.needTarget"
                shell.BorderBrush <- Ui.brush p.Warn
                lastValue <- None
            | Some t ->
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                shell.BorderBrush <- Ui.brush (if fault then p.Error else orElse edge vm.BorderColor)
                match wordValue status t with
                | Some v ->
                    lastValue <- Some v
                    valueText.Text <- formatValue vm v
                | None ->
                    lastValue <- None
                    valueText.Text <- "----"
                valueText.Foreground <- Ui.brush (if fault then p.Error else orElse ink vm.TextColor)
                subText.Text <-
                    match host.Resolve vm.SubTargetId with
                    | Some s ->
                        match wordValue status s with
                        | Some v -> formatValue vm v
                        | None -> "----"
                    | None -> ""

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> shell.CornerRadius <- cornerOf vm }

    // ---------------- 아날로그 계기 ----------------
    | PartGauge ->
        let face = GaugeFace()
        face.TrackColor <- orElse (mix background ink 0.16) vm.OffColor
        face.ArcColor <- orElse p.Accent vm.OnColor
        face.NeedleColor <- orElse ink vm.TextColor

        let valueText = Ui.mono (float vm.FontSize * 1.5) "----"
        valueText.FontWeight <- FontWeight.Bold
        valueText.Foreground <- Ui.brush (orElse ink vm.TextColor)
        valueText.HorizontalAlignment <- HorizontalAlignment.Center
        valueText.VerticalAlignment <- VerticalAlignment.Bottom

        let caption = Ui.text (captionOf host vm)
        caption.FontSize <- float vm.FontSize
        caption.Foreground <- Ui.brush dim
        caption.HorizontalAlignment <- HorizontalAlignment.Center
        caption.VerticalAlignment <- VerticalAlignment.Bottom
        caption.TextTrimming <- TextTrimming.CharacterEllipsis

        // 설정값 다이얼로 쓸 때 아래에 현재값(보조 요소)을 같이 보여 준다.
        let subText = Ui.mono (max 10.0 (float vm.FontSize * 0.95)) ""
        subText.Foreground <- Ui.brush dim
        subText.HorizontalAlignment <- HorizontalAlignment.Center

        let readout = Ui.stackV 0.0 [ valueText; subText; caption ]
        readout.HorizontalAlignment <- HorizontalAlignment.Center
        readout.VerticalAlignment <- VerticalAlignment.Bottom
        readout.Margin <- Thickness(0.0, 0.0, 0.0, 2.0)

        let stack = Grid()
        stack.Children.Add face
        stack.Children.Add readout
        root.Child <- stack

        /// 숫자입력 요소를 연결했을 때만 손으로 돌릴 수 있다. (숫자표시는 읽기 전용)
        let adjustable () =
            touchable ()
            && (match target () with
                | Some t ->
                    let lo, hi = scaleOf vm t
                    t.Kind = NumInput && hi > lo
                | None -> false)

        // 돌리는 동안에는 폴링이 바늘을 도로 끌어가지 않게 막는다.
        let mutable dragging = false
        let mutable pending = 0

        let previewAt (t: ElementVm) (position: Point) =
            let lo, hi = scaleOf vm t
            match ratioAtPoint face.Bounds.Size position with
            | Some ratio when hi > lo ->
                // 눈금 범위 안에서 값을 고르고, 실제로 쓸 때는 요소 범위로 한 번 더 자른다.
                let v = max lo (min hi (lo + int (Math.Round(ratio * float (hi - lo)))))
                pending <- v
                face.Ratio <- Some(float (v - lo) / float (hi - lo))
                valueText.Text <- formatValue vm v
                face.InvalidateVisual()
            | _ -> ()

        root.PointerPressed.Add(fun e ->
            if adjustable () && e.GetCurrentPoint(root).Properties.IsLeftButtonPressed then
                match target () with
                | Some t ->
                    dragging <- true
                    previewAt t (e.GetPosition face)
                    e.Pointer.Capture root
                    e.Handled <- true
                | None -> ())

        root.PointerMoved.Add(fun e ->
            if dragging then
                match target () with
                | Some t -> previewAt t (e.GetPosition face)
                | None -> ())

        // 돌리는 동안에는 쓰지 않는다. 손을 뗄 때 한 번만 보낸다.
        // (끄는 내내 쓰면 PLC 로 수십 번 나간다)
        root.PointerReleased.Add(fun e ->
            if dragging then
                dragging <- false
                e.Pointer.Capture null
                match target () with
                | Some t when adjustable () -> host.Cards.NumericWrite t (max t.Min (min t.Max pending))
                | _ -> ())

        root.Cursor <- new Cursor(StandardCursorType.Hand)

        let refresh (status: CardFactory.RuntimeStatus) =
            caption.Text <- captionOf host vm
            match target () with
            | None ->
                face.Ratio <- None
                valueText.Text <- "----"
                subText.Text <- ""
                caption.Text <- I18n.t "hmi.needTarget"
            | Some t ->
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                face.ArcColor <- (if fault then p.Error else orElse p.Accent vm.OnColor)
                // 손으로 돌리는 중이면 그 바늘을 그대로 둔다.
                if not dragging then
                    let lo, hi = scaleOf vm t
                    match wordValue status t with
                    | Some v ->
                        let span = float (hi - lo)
                        face.Ratio <-
                            (if span > 0.0 then Some(max 0.0 (min 1.0 (float (v - lo) / span))) else Some 0.0)
                        valueText.Text <- formatValue vm v
                    | None ->
                        face.Ratio <- None
                        valueText.Text <- "----"
                subText.Text <-
                    match host.Resolve vm.SubTargetId with
                    | Some sub ->
                        match wordValue status sub with
                        | Some v -> formatValue vm v
                        | None -> "----"
                    | None -> ""
            face.InvalidateVisual()

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> face.InvalidateVisual() }

    // ---------------- 로터리 셀렉터 ----------------
    | PartRotary ->
        let count = max 2 vm.Count
        let labels =
            let given = (if isNull vm.Options then "" else vm.Options).Split '|'
            Array.init count (fun i -> if i < given.Length then given.[i].Trim() else string i)

        let dial = RotaryFace(Count = count)
        dial.DialColor <- orElse (mix background ink 0.20) vm.OffColor
        dial.KnobColor <- orElse "#C8CDD6" vm.OnColor
        dial.MarkColor <- ink
        dial.TickColor <- dim

        let caption = Ui.text (captionOf host vm)
        caption.FontSize <- float vm.FontSize
        caption.Foreground <- Ui.brush dim
        caption.HorizontalAlignment <- HorizontalAlignment.Center
        caption.VerticalAlignment <- VerticalAlignment.Bottom
        caption.TextTrimming <- TextTrimming.CharacterEllipsis

        let stack = Grid()
        stack.Children.Add dial
        stack.Children.Add caption
        root.Child <- stack
        root.Cursor <- new Cursor(StandardCursorType.Hand)

        let mutable position = 0

        // 누를 때마다 다음 자리로 돈다. 마지막 자리에서는 처음으로 되돌아온다.
        root.PointerReleased.Add(fun _ ->
            if touchable () then
                match target () with
                | Some t ->
                    let next = (position + 1) % count
                    host.Cards.NumericWrite t (max t.Min (min t.Max next))
                | None -> ())

        let refresh (status: CardFactory.RuntimeStatus) =
            match target () with
            | None ->
                dial.Position <- None
                caption.Text <- I18n.t "hmi.needTarget"
            | Some t ->
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                match wordValue status t with
                | Some v ->
                    position <- max 0 (min (count - 1) v)
                    dial.Position <- Some position
                    caption.Text <-
                        let name = captionOf host vm
                        if String.IsNullOrWhiteSpace name then labels.[position]
                        else name + "  ·  " + labels.[position]
                | None ->
                    dial.Position <- None
                    caption.Text <- captionOf host vm
                dial.MarkColor <- (if fault then p.Error else ink)
            dial.InvalidateVisual()

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> dial.InvalidateVisual() }

    // ---------------- 삼각 증감 버튼 ----------------
    | PartArrow ->
        let up = vm.Step >= 0
        let accent = orElse (if up then "#E23B4E" else "#2D7FF9") vm.OnColor

        let shell =
            Border(
                Background = Ui.brush (orElse "#00000000" vm.OffColor),
                BorderBrush = Ui.brush (orElse "#00000000" vm.BorderColor),
                BorderThickness = Thickness 1.5,
                CornerRadius = CornerRadius(float vm.Corner),
                ClipToBounds = true
            )

        // 삼각형은 부품 크기에 맞춰 늘어난다.
        let triangle =
            Avalonia.Controls.Shapes.Path(
                Fill = Ui.brush accent,
                Stretch = Stretch.Uniform,
                Margin = Thickness 8.0,
                Data =
                    Geometry.Parse(if up then "M 50,0 L 100,86 L 0,86 Z" else "M 0,0 L 100,0 L 50,86 Z"))
        shell.Child <- triangle
        root.Child <- shell
        root.Cursor <- new Cursor(StandardCursorType.Hand)

        let mutable lastValue: int option = None
        let mutable held = false

        let paint () = triangle.Opacity <- (if held then 0.55 else 1.0)

        root.PointerPressed.Add(fun _ ->
            if touchable () then
                held <- true
                paint ())

        root.PointerExited.Add(fun _ ->
            if held then
                held <- false
                paint ())

        root.PointerReleased.Add(fun _ ->
            if held then
                held <- false
                paint ()
                if touchable () then
                    match target () with
                    | Some t when t.Kind = NumInput ->
                        let current = lastValue |> Option.defaultValue t.Min
                        let next = max t.Min (min t.Max (current + vm.Step))
                        if next <> current then host.Cards.NumericWrite t next
                    | _ -> ())

        let refresh (status: CardFactory.RuntimeStatus) =
            match target () with
            | None ->
                lastValue <- None
                triangle.Fill <- Ui.brush dim
                shell.BorderBrush <- Ui.brush p.Warn
            | Some t ->
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                lastValue <- wordValue status t
                triangle.Fill <- Ui.brush (if fault then p.Error else accent)
                shell.BorderBrush <- Ui.brush (orElse "#00000000" vm.BorderColor)

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> shell.CornerRadius <- CornerRadius(float vm.Corner) }

    // ---------------- 값 지정 버튼 (프리셋) ----------------
    | PartSetValue ->
        let shell =
            Border(
                Background = Ui.brush defaultOff,
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 2.0,
                CornerRadius = cornerOf vm,
                ClipToBounds = true
            )

        let caption = Ui.title (float vm.FontSize) ""
        caption.TextAlignment <- TextAlignment.Center
        caption.TextWrapping <- TextWrapping.Wrap
        caption.HorizontalAlignment <- HorizontalAlignment.Center
        caption.VerticalAlignment <- VerticalAlignment.Center
        caption.Margin <- Thickness 6.0
        shell.Child <- caption
        root.Child <- shell
        root.Cursor <- new Cursor(StandardCursorType.Hand)

        /// 글자를 비워 두면 쓸 값을 그대로 보여 준다.
        let text () =
            if not (String.IsNullOrWhiteSpace vm.Text) then vm.Text
            else formatValue vm vm.WriteValue

        let paint (isOn: bool) (fault: bool) =
            let fill = if fault then p.Error elif isOn then defaultOn else defaultOff
            shell.Background <- Ui.brush fill
            shell.BoxShadow <- (if isOn && not fault then glow fill (float vm.Height * 0.28) else BoxShadows())
            caption.Foreground <- Ui.brush (orElse (textOn fill) vm.TextColor)
            caption.Text <- text ()

        root.PointerReleased.Add(fun _ ->
            if touchable () then
                match target () with
                | Some t when t.Kind = NumInput ->
                    resetGroupPeers ()
                    host.Cards.NumericWrite t (max t.Min (min t.Max vm.WriteValue))
                | _ -> ())

        let refresh (status: CardFactory.RuntimeStatus) =
            match target () with
            | None ->
                shell.BorderBrush <- Ui.brush p.Warn
                paint false false
                caption.Text <- I18n.t "hmi.needTarget"
                caption.Foreground <- Ui.brush dim
            | Some t ->
                shell.BorderBrush <- Ui.brush (orElse edge vm.BorderColor)
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                // 지금 값이 이 버튼의 값과 같으면 '고른 상태' 로 점등한다.
                let current = wordValue status t
                paint (current = Some vm.WriteValue) fault
                shell.Opacity <- (if current.IsNone && not fault && not (host.Editing ()) then 0.55 else 1.0)

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize = fun () -> shell.CornerRadius <- cornerOf vm }

    // ---------------- 램프 배열 ----------------
    | PartLampArray ->
        let count = max 1 vm.Count
        let shell =
            Border(
                Background = Ui.brush (orElse (mix background ink 0.07) vm.OffColor),
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 1.5,
                CornerRadius = CornerRadius(float vm.Corner),
                Padding = Thickness(8.0, 6.0, 8.0, 6.0)
            )

        let lamps = ResizeArray<Border>()
        let strip =
            let panel =
                StackPanel(
                    Orientation = (if vm.Vertical then Orientation.Vertical else Orientation.Horizontal),
                    Spacing = 6.0,
                    HorizontalAlignment = HorizontalAlignment.Center,
                    VerticalAlignment = VerticalAlignment.Center)
            for i in 1..count do
                let dot =
                    Border(
                        CornerRadius = CornerRadius 999.0,
                        BorderBrush = Ui.brush edge,
                        BorderThickness = Thickness 1.5,
                        Background = Ui.brush (mix background ink 0.16),
                        HorizontalAlignment = HorizontalAlignment.Center)
                lamps.Add dot
                let number = Ui.text (string i)
                number.FontSize <- float vm.FontSize
                number.Foreground <- Ui.brush dim
                number.HorizontalAlignment <- HorizontalAlignment.Center
                let cell = Ui.stackV 2.0 [ dot; number ]
                cell.HorizontalAlignment <- HorizontalAlignment.Center
                panel.Children.Add cell
            panel

        let caption = Ui.text (captionOf host vm)
        caption.FontSize <- float vm.FontSize
        caption.Foreground <- Ui.brush dim
        caption.Margin <- Thickness(0.0, 0.0, 0.0, 2.0)

        let body = DockPanel(LastChildFill = true)
        if not (String.IsNullOrWhiteSpace vm.Text) then
            DockPanel.SetDock(caption, Dock.Top)
            body.Children.Add caption
        body.Children.Add strip
        shell.Child <- body
        root.Child <- shell

        /// 표시등 크기는 부품 크기에 맞춰 잡는다.
        let sizeLamps () =
            let along = if vm.Vertical then float vm.Height else float vm.Width
            let across = if vm.Vertical then float vm.Width else float vm.Height
            let byLength = (along - 24.0) / float count - 8.0
            let byHeight = across - 20.0 - (float vm.FontSize + 6.0)
            let size = max 8.0 (min byLength byHeight)
            for dot in lamps do
                dot.Width <- size
                dot.Height <- size

        let refresh (status: CardFactory.RuntimeStatus) =
            sizeLamps ()
            match target () with
            | None ->
                for dot in lamps do
                    dot.Background <- Ui.brush (mix background ink 0.16)
                shell.BorderBrush <- Ui.brush p.Warn
            | Some t ->
                shell.BorderBrush <- Ui.brush (orElse edge vm.BorderColor)
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                let onColor = orElse p.On vm.OnColor
                let offColor = mix background ink 0.16
                for i in 0 .. count - 1 do
                    // 연결한 요소의 주소에서 i 만큼 뒤의 비트. WORD 경계를 넘으면 다음 WORD 로 간다.
                    let address =
                        try Some(Address.offsetBit t.Device i) with _ -> None
                    let live = address |> Option.bind (status.BitOf t.PlcId)
                    let dot = lamps.[i]
                    if fault then
                        dot.Background <- Ui.brush p.Error
                        dot.BoxShadow <- BoxShadows()
                    else
                        match live with
                        | Some true ->
                            dot.Background <- Ui.brush onColor
                            dot.BoxShadow <- glow onColor (dot.Width * 0.5)
                        | _ ->
                            dot.Background <- Ui.brush offColor
                            dot.BoxShadow <- BoxShadows()
                    dot.Opacity <- (if live.IsNone && not fault && not (host.Editing ()) then 0.5 else 1.0)

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize =
            fun () ->
                shell.CornerRadius <- CornerRadius(float vm.Corner)
                sizeLamps () }

    // ---------------- 막대 그래프 ----------------
    | PartBar ->
        let track =
            Border(
                Background = Ui.brush (orElse (mix background ink 0.12) vm.OffColor),
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 1.5,
                CornerRadius = CornerRadius(float vm.Corner),
                ClipToBounds = true)

        let fill =
            Border(
                Background = Ui.brush (orElse p.Accent vm.OnColor),
                HorizontalAlignment = (if vm.Vertical then HorizontalAlignment.Stretch else HorizontalAlignment.Left),
                VerticalAlignment = (if vm.Vertical then VerticalAlignment.Bottom else VerticalAlignment.Stretch))

        let valueText = Ui.mono (float vm.FontSize) "----"
        valueText.FontWeight <- FontWeight.Bold
        valueText.Foreground <- Ui.brush (orElse ink vm.TextColor)
        valueText.HorizontalAlignment <- HorizontalAlignment.Center
        valueText.VerticalAlignment <- VerticalAlignment.Center

        let stack = Grid()
        stack.Children.Add fill
        stack.Children.Add valueText
        track.Child <- stack
        root.Child <- track

        let apply (ratio: float option) =
            match ratio with
            | Some r ->
                let r = max 0.0 (min 1.0 r)
                fill.IsVisible <- true
                if vm.Vertical then
                    fill.Width <- Double.NaN
                    fill.Height <- max 0.0 (float vm.Height - 3.0) * r
                else
                    fill.Height <- Double.NaN
                    fill.Width <- max 0.0 (float vm.Width - 3.0) * r
            | None -> fill.IsVisible <- false

        let mutable lastRatio: float option = None

        let refresh (status: CardFactory.RuntimeStatus) =
            match target () with
            | None ->
                lastRatio <- None
                apply None
                valueText.Text <- I18n.t "hmi.needTarget"
                valueText.Foreground <- Ui.brush dim
                track.BorderBrush <- Ui.brush p.Warn
            | Some t ->
                let fault = status.CommFault t.PlcId || t.Fault.IsSome
                track.BorderBrush <- Ui.brush (if fault then p.Error else orElse edge vm.BorderColor)
                fill.Background <- Ui.brush (if fault then p.Error else orElse p.Accent vm.OnColor)
                let lo, hi = scaleOf vm t
                match wordValue status t with
                | Some v ->
                    let span = float (hi - lo)
                    lastRatio <-
                        (if span > 0.0 then Some(max 0.0 (min 1.0 (float (v - lo) / span))) else Some 0.0)
                    valueText.Text <- formatValue vm v
                | None ->
                    lastRatio <- None
                    valueText.Text <- "----"
                valueText.Foreground <- Ui.brush (orElse ink vm.TextColor)
                apply lastRatio

        { Vm = vm
          Root = root
          Refresh = refresh
          Resize =
            fun () ->
                track.CornerRadius <- CornerRadius(float vm.Corner)
                apply lastRatio }

    // ---------------- 시계 ----------------
    | PartClock ->
        let text = Ui.title (float vm.FontSize) ""
        text.TextAlignment <- textAlignOf vm.Align
        text.HorizontalAlignment <- hAlignOf vm.Align
        text.VerticalAlignment <- VerticalAlignment.Center
        text.Foreground <- Ui.brush (orElse ink vm.TextColor)
        root.Child <- text

        /// 글자 칸에 적어 둔 것을 .NET 날짜 형식으로 쓴다. 비우면 기본 형식.
        let format = if String.IsNullOrWhiteSpace vm.Text then "yyyy-MM-dd  tt h:mm:ss" else vm.Text

        let tick () =
            text.Text <-
                try DateTime.Now.ToString format
                with _ -> DateTime.Now.ToString "yyyy-MM-dd  tt h:mm:ss"

        tick ()

        // PLC 와 무관하게 스스로 돈다. 화면에서 떨어지면 반드시 멈춘다. (누수 방지)
        let timer = Threading.DispatcherTimer(Interval = TimeSpan.FromMilliseconds 500.0)
        timer.Tick.Add(fun _ -> tick ())
        root.AttachedToVisualTree.Add(fun _ -> timer.Start())
        root.DetachedFromVisualTree.Add(fun _ -> timer.Stop())

        { Vm = vm
          Root = root
          Refresh = (fun _ -> tick ())
          Resize = ignore }

    // ---------------- 글자 ----------------
    | PartLabel ->
        let text = Ui.title (float vm.FontSize) vm.Text
        text.TextWrapping <- TextWrapping.Wrap
        text.TextAlignment <- textAlignOf vm.Align
        text.HorizontalAlignment <- hAlignOf vm.Align
        text.VerticalAlignment <- VerticalAlignment.Center
        text.Foreground <- Ui.brush (orElse ink vm.TextColor)
        root.Child <- text

        { Vm = vm
          Root = root
          Refresh = (fun _ -> text.Text <- vm.Text)
          Resize = ignore }

    // ---------------- 패널 (배경 구획) ----------------
    | PartPanel ->
        let shell =
            Border(
                Background = Ui.brush (orElse (mix background ink 0.09) vm.OffColor),
                BorderBrush = Ui.brush (orElse edge vm.BorderColor),
                BorderThickness = Thickness 1.5,
                CornerRadius = CornerRadius(float vm.Corner)
            )

        let caption = Ui.title (float vm.FontSize) vm.Text
        caption.Margin <- Thickness(14.0, 8.0, 14.0, 0.0)
        caption.VerticalAlignment <- VerticalAlignment.Top
        caption.HorizontalAlignment <- hAlignOf vm.Align
        caption.Foreground <- Ui.brush (orElse dim vm.TextColor)
        shell.Child <- caption
        root.Child <- shell

        { Vm = vm
          Root = root
          Refresh = (fun _ -> caption.Text <- vm.Text)
          Resize = fun () -> shell.CornerRadius <- CornerRadius(float vm.Corner) }
