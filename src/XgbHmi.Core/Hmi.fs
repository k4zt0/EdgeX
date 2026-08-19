namespace XgbHmi.Core

open System

/// 터치스크린(HMI) 화면에 놓는 부품 종류.
/// 부품은 제 주소를 갖지 않는다. 화면 편집에 있는 요소(HmiItem)를 연결해서 그 주소로 동작한다.
/// 주소·동작·검증 규칙이 한 군데(HmiItem)에만 있어야 설비 동작이 어긋나지 않는다.
type HmiPartKind =
    /// 누르면 연결한 요소의 스위치 동작(토글/ON/OFF/순간)을 수행한다.
    | PartButton
    /// 연결한 요소의 비트를 점등으로 보여 준다. 누를 수 없다.
    | PartLamp
    /// 노브가 좌우로 움직이는 셀렉터 스위치. 누르면 토글한다.
    | PartToggle
    /// 연결한 요소의 WORD 값. 숫자입력이면 눌러서 키패드로 쓰고 ▲▼로 올리고 내린다.
    | PartValue
    /// 바늘이 도는 아날로그 계기. 연결한 요소의 Min~Max 를 눈금으로 쓴다.
    | PartGauge
    /// 여러 자리를 도는 로터리 셀렉터. 자리 번호를 연결한 WORD 에 쓴다.
    | PartRotary
    /// 삼각(▲▼) 증감 버튼. 누를 때마다 연결한 WORD 를 증감폭만큼 올리고 내린다.
    | PartArrow
    /// 정해 둔 값을 그대로 쓰는 버튼. 지금 값과 같으면 점등한다. (프리셋)
    | PartSetValue
    /// 연속한 비트를 한 줄로 늘어놓은 표시등 묶음. (Controller Inputs/Outputs 같은 것)
    | PartLampArray
    /// 값을 채워 보여 주는 막대 그래프
    | PartBar
    /// 날짜·시각 (PLC 와 무관)
    | PartClock
    /// 글자만 (연결 없음)
    | PartLabel
    /// 배경 상자 / 구획 (연결 없음)
    | PartPanel

    member this.Code =
        match this with
        | PartButton -> "BUTTON"
        | PartLamp -> "LAMP"
        | PartToggle -> "TOGGLE"
        | PartValue -> "VALUE"
        | PartGauge -> "GAUGE"
        | PartRotary -> "ROTARY"
        | PartArrow -> "ARROW"
        | PartSetValue -> "SET_VALUE"
        | PartLampArray -> "LAMP_ARRAY"
        | PartBar -> "BAR"
        | PartClock -> "CLOCK"
        | PartLabel -> "LABEL"
        | PartPanel -> "PANEL"


[<RequireQualifiedAccess>]
module HmiPartKind =

    let all =
        [ PartButton
          PartLamp
          PartToggle
          PartRotary
          PartArrow
          PartSetValue
          PartValue
          PartGauge
          PartBar
          PartLampArray
          PartClock
          PartLabel
          PartPanel ]

    let tryParse (s: string) =
        match (if isNull s then "" else s.Trim().ToUpperInvariant()) with
        | "BUTTON" -> Some PartButton
        | "LAMP" -> Some PartLamp
        | "TOGGLE" -> Some PartToggle
        | "VALUE" -> Some PartValue
        | "GAUGE" -> Some PartGauge
        | "ROTARY" -> Some PartRotary
        | "ARROW" -> Some PartArrow
        | "SET_VALUE" -> Some PartSetValue
        | "LAMP_ARRAY" -> Some PartLampArray
        | "BAR" -> Some PartBar
        | "CLOCK" -> Some PartClock
        | "LABEL" -> Some PartLabel
        | "PANEL" -> Some PartPanel
        | _ -> None

    /// 비트(M/P) 요소를 연결하는 부품인지
    let isBitPart kind =
        kind = PartButton || kind = PartLamp || kind = PartToggle || kind = PartLampArray

    /// D WORD 요소를 연결하는 부품인지
    let isWordPart kind =
        kind = PartValue || kind = PartGauge || kind = PartBar || kind = PartRotary
        || kind = PartArrow || kind = PartSetValue

    /// 화면 요소를 연결해야 쓸모가 있는 부품인지
    let needsTarget kind = isBitPart kind || isWordPart kind

    /// 손으로 눌러 PLC 로 명령을 보내는 부품인지.
    /// 계기는 숫자입력 요소를 연결했을 때만 실제로 돌아간다. (읽기 전용에는 쓰지 않는다)
    let isTouchable kind =
        kind = PartButton || kind = PartToggle || kind = PartValue || kind = PartRotary
        || kind = PartArrow || kind = PartSetValue || kind = PartGauge

    /// 연결한 요소의 '스위치 동작' 을 부품에서 덮어쓸 수 있는지
    let hasAction kind = kind = PartButton || kind = PartToggle

    /// 상호 배타 그룹에 넣을 수 있는 부품인지
    let hasGroup kind = kind = PartButton || kind = PartToggle || kind = PartSetValue

    /// 모양(사각/알약/원)을 고를 수 있는 부품인지
    let hasShape kind = kind = PartButton || kind = PartLamp || kind = PartSetValue

    /// 소수점 자리수를 쓰는 부품인지
    let hasDecimals kind = kind = PartValue || kind = PartGauge || kind = PartBar

    /// 눈금 범위를 따로 줄 수 있는 부품인지
    let hasScale kind = kind = PartGauge || kind = PartBar

    /// 가로/세로를 고를 수 있는 부품인지
    let hasOrientation kind = kind = PartBar || kind = PartLampArray


/// 부품 겉모양.
[<RequireQualifiedAccess>]
module HmiShape =
    let rect = "RECT"
    let round = "ROUND"
    let circle = "CIRCLE"
    let all = [ rect; round; circle ]

    let normalize (s: string) =
        let s = if isNull s then "" else s.Trim().ToUpperInvariant()
        if all |> List.contains s then s else rect


/// 터치스크린 부품 한 개.
type HmiPart =
    { Id: string
      Kind: HmiPartKind
      /// 조작하거나 상태를 보여 줄 화면 요소의 Id. (라벨/패널은 빈 문자열)
      TargetId: string
      /// 값 부품이 작은 글씨로 함께 보여 줄 두 번째 요소. (설정값 아래 현재값)
      SubTargetId: string
      /// 부품에 쓸 글자. 비우면 연결한 요소의 이름을 쓴다.
      Text: string
      /// 켜짐 / 꺼짐일 때 바꿔 쓸 글자. 비우면 Text 를 그대로 쓴다.
      OnText: string
      OffText: string
      /// 값/계기 옆에 붙일 단위
      Unit: string
      X: int
      Y: int
      Width: int
      Height: int
      /// RECT / ROUND / CIRCLE
      Shape: string
      /// 꺼짐 / 켜짐 색 (#RRGGBB). 비우면 테마 기본색을 쓴다.
      OffColor: string
      OnColor: string
      TextColor: string
      BorderColor: string
      FontSize: int
      /// 모서리 둥글기 (px). Shape 가 RECT 일 때만 쓴다.
      Corner: int
      /// LEFT / CENTER / RIGHT
      Align: string
      /// 값 부품의 ▲▼ 증감폭. 0 이면 화살표를 두지 않는다.
      /// 삼각 버튼은 이 값만큼 올리고 내린다. (음수면 내리는 버튼)
      Step: int
      /// 연결한 요소의 스위치 동작을 덮어쓴다. 비우면 요소에 설정된 동작을 그대로 쓴다.
      /// 같은 코일에 '운전'(ON)과 '정지'(OFF) 버튼을 따로 두는 데 쓴다.
      /// 값은 SwitchAction.Code 와 같아야 한다. (토글 / ON / OFF / 순간)
      Action: string
      /// 램프 배열의 표시등 개수 / 로터리 셀렉터의 자리 수
      Count: int
      /// 소수점 자리수. 1 이면 PLC 값 80 을 8.0 으로 보여 준다.
      Decimals: int
      /// 값 지정 버튼이 쓸 값
      WriteValue: int
      /// 로터리 셀렉터의 자리 이름. '|' 로 나눈다. (예: "LOW|HIGH")
      Options: string
      /// 세로로 놓을지. (막대 그래프 / 램프 배열)
      Vertical: bool
      /// 계기·막대의 눈금 범위. ScaleMax 가 ScaleMin 보다 클 때만 쓰고,
      /// 아니면 연결한 요소의 최소~최대를 그대로 쓴다.
      /// (요소의 범위는 PLC 데이터 범위이고, 이쪽은 화면에 보여 줄 공정 범위다)
      ScaleMin: int
      ScaleMax: int
      /// 누른 뒤 ON 으로 만들 요소. (재시작처럼 '명령' 이라서 누른 뒤 다른 버튼이 켜져야 할 때)
      /// 본 동작을 마친 다음에 쓴다.
      ThenOnId: string
      /// 상호 배타 버튼 그룹 이름. 같은 이름끼리 한 번에 하나만 켜진다.
      /// 하나를 누르면 같은 그룹의 다른 버튼이 가리키는 코일을 먼저 OFF 로 쓴다.
      /// (실행 / 중지 / 종료처럼 동시에 켜지면 안 되는 조작에 쓴다)
      Group: string }


/// 터치스크린 화면 한 장.
type HmiScreen =
    { /// 실제 터치패널 해상도 (부품 좌표의 기준)
      Width: int
      Height: int
      /// 화면 바탕색 (#RRGGBB). 비우면 테마 기본
      Background: string
      Parts: HmiPart list }


[<RequireQualifiedAccess>]
module HmiLimits =
    let minPartWidth = 30
    let minPartHeight = 24
    let minScreen = 320
    let maxScreen = 4096
    let defaultWidth = 1024
    let defaultHeight = 600
    let minFontSize = 8
    let maxFontSize = 96

    /// 흔한 터치패널 해상도. 화면 크기 고르기에 쓴다.
    let presets =
        [ 800, 480
          1024, 600
          1024, 768
          1280, 800
          1366, 768
          1920, 1080 ]


[<RequireQualifiedAccess>]
module HmiPart =

    let newId () = Guid.NewGuid().ToString("N")

    let alignments = [ "LEFT"; "CENTER"; "RIGHT" ]

    /// 부품에서 고를 수 있는 스위치 동작. 빈 문자열은 '연결한 요소 설정 그대로'.
    /// SwitchAction.Code 와 같은 문자열이어야 한다. (테스트가 이 둘을 맞춰 준다)
    let actionCodes = [ ""; "토글"; "ON"; "OFF"; "순간" ]

    let normalizeAction (s: string) =
        let s = if isNull s then "" else s.Trim()
        if actionCodes |> List.contains s then s else ""

    let normalizeAlign (s: string) =
        let s = if isNull s then "" else s.Trim().ToUpperInvariant()
        if alignments |> List.contains s then s else "CENTER"

    /// #RRGGBB 만 통과시킨다. 아니면 빈 문자열(테마 기본)로 돌린다.
    let normalizeColor (s: string) =
        let s = if isNull s then "" else s.Trim()
        if s.Length = 7 && s.[0] = '#' && s.Substring 1 |> Seq.forall Uri.IsHexDigit then s.ToUpperInvariant()
        else ""

    let create kind =
        let common =
            { Id = newId ()
              Kind = kind
              TargetId = ""
              SubTargetId = ""
              Text = ""
              OnText = ""
              OffText = ""
              Unit = ""
              X = 40
              Y = 40
              Width = 180
              Height = 90
              Shape = HmiShape.rect
              OffColor = ""
              OnColor = ""
              TextColor = ""
              BorderColor = ""
              FontSize = 18
              Corner = 8
              Align = "CENTER"
              Step = 0
              Action = ""
              Count = 8
              Decimals = 0
              WriteValue = 0
              Options = ""
              Vertical = false
              ScaleMin = 0
              ScaleMax = 0
              ThenOnId = ""
              Group = "" }
        match kind with
        | PartButton -> { common with Width = 110; Height = 110; Shape = HmiShape.circle; FontSize = 17 }
        | PartLamp -> { common with Width = 96; Height = 96; Shape = HmiShape.circle; FontSize = 15 }
        | PartToggle -> { common with Width = 150; Height = 70; FontSize = 13; OnText = "ON"; OffText = "OFF" }
        | PartValue -> { common with Width = 230; Height = 110; FontSize = 40; Step = 1 }
        | PartGauge -> { common with Width = 190; Height = 190; FontSize = 15 }
        | PartRotary -> { common with Width = 150; Height = 150; FontSize = 12; Count = 2; Options = "LOW|HIGH" }
        | PartArrow -> { common with Width = 62; Height = 58; FontSize = 12; Step = 1 }
        | PartSetValue -> { common with Width = 130; Height = 62; FontSize = 16; Corner = 8; WriteValue = 0 }
        | PartLampArray -> { common with Width = 260; Height = 62; FontSize = 11; Count = 8 }
        | PartBar -> { common with Width = 280; Height = 54; FontSize = 14; Corner = 6 }
        | PartClock -> { common with Width = 210; Height = 40; FontSize = 16; Align = "RIGHT" }
        | PartLabel ->
            { common with
                Text = "제목"
                Width = 260
                Height = 46
                FontSize = 20
                Corner = 0
                Align = "LEFT" }
        | PartPanel ->
            { common with
                Width = 420
                Height = 260
                FontSize = 15
                Corner = 20
                Align = "LEFT" }

    let clone (fresh: bool) (src: HmiPart) =
        if fresh then { src with Id = newId () } else src

    let normalize (p: HmiPart) =
        let str (s: string) = if isNull s then "" else s
        { p with
            TargetId = (str p.TargetId).Trim()
            SubTargetId = (str p.SubTargetId).Trim()
            Text = str p.Text
            OnText = str p.OnText
            OffText = str p.OffText
            Unit = str p.Unit
            X = max 0 p.X
            Y = max 0 p.Y
            Width = max HmiLimits.minPartWidth p.Width
            Height = max HmiLimits.minPartHeight p.Height
            Shape = HmiShape.normalize p.Shape
            OffColor = normalizeColor p.OffColor
            OnColor = normalizeColor p.OnColor
            TextColor = normalizeColor p.TextColor
            BorderColor = normalizeColor p.BorderColor
            FontSize = max HmiLimits.minFontSize (min HmiLimits.maxFontSize p.FontSize)
            Corner = max 0 (min 60 p.Corner)
            Align = normalizeAlign p.Align
            Step = max -10000 (min 10000 p.Step)
            Action = normalizeAction p.Action
            Count = max 1 (min 16 p.Count)
            Decimals = max 0 (min 3 p.Decimals)
            WriteValue = max -32768 (min 65535 p.WriteValue)
            Options = str p.Options
            ScaleMin = max -32768 (min 65535 p.ScaleMin)
            ScaleMax = max -32768 (min 65535 p.ScaleMax)
            ThenOnId = (str p.ThenOnId).Trim()
            Group = (str p.Group).Trim() }


[<RequireQualifiedAccess>]
module HmiScreen =

    let empty =
        { Width = HmiLimits.defaultWidth
          Height = HmiLimits.defaultHeight
          Background = ""
          Parts = [] }

    let normalize (s: HmiScreen) =
        { s with
            Width = max HmiLimits.minScreen (min HmiLimits.maxScreen s.Width)
            Height = max HmiLimits.minScreen (min HmiLimits.maxScreen s.Height)
            Background = HmiPart.normalizeColor s.Background
            Parts = s.Parts |> List.map HmiPart.normalize }

    /// 새 부품을 화면 안 빈 자리에 놓는다. (겹치면 조금씩 어긋나게)
    let nextFreePosition (parts: HmiPart list) (screenWidth: int) (screenHeight: int) (width: int) (height: int) =
        let step = 26
        let mutable x = 40
        let mutable y = 40
        let mutable guard = 0
        let overlaps px py =
            parts |> List.exists (fun q -> abs (q.X - px) < step && abs (q.Y - py) < step)
        while overlaps x y && guard < 200 do
            x <- x + step
            y <- y + step
            if x + width > screenWidth - 20 || y + height > screenHeight - 20 then
                x <- 40 + (guard % 5) * 18
                y <- 40 + (guard % 7) * 18
            guard <- guard + 1
        x, y
