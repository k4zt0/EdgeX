namespace XgbHmi.Core

open System

/// 화면 요소 종류. XML/그리드에서는 기존 v6 파일과 같은 문자열 코드로 저장한다.
type ItemKind =
    | Switch
    | Lamp
    /// 조작 버튼과 램프를 한 장에 합친 것. 누르면서 상태도 함께 본다.
    | SwitchLamp
    /// 화면에 있는 요소를 골라 모두 조작할 수 있는 스위치 한 개.
    /// 어떤 조작이 도는지, 잘 됐는지까지 이 한 장에서 본다.
    | MasterSwitch
    | NumInput
    | NumDisplay
    | Text

    member this.Code =
        match this with
        | Switch -> "SWITCH"
        | Lamp -> "LAMP"
        | SwitchLamp -> "SWITCH_LAMP"
        | MasterSwitch -> "MASTER_SWITCH"
        | NumInput -> "NUM_INPUT"
        | NumDisplay -> "NUM_DISPLAY"
        | Text -> "TEXT"

    member this.Label =
        match this with
        | Switch -> "스위치"
        | Lamp -> "램프"
        | SwitchLamp -> "스위치/램프"
        | MasterSwitch -> "통합 스위치"
        | NumInput -> "숫자입력"
        | NumDisplay -> "숫자표시"
        | Text -> "텍스트"

[<RequireQualifiedAccess>]
module ItemKind =

    let all = [ Switch; Lamp; SwitchLamp; MasterSwitch; NumInput; NumDisplay; Text ]

    let codes = all |> List.map (fun k -> k.Code)

    let tryParse (s: string) =
        match (if isNull s then "" else s.Trim().ToUpperInvariant()) with
        | "SWITCH" -> Some Switch
        | "LAMP" -> Some Lamp
        | "SWITCH_LAMP" -> Some SwitchLamp
        | "MASTER_SWITCH" -> Some MasterSwitch
        | "NUM_INPUT" -> Some NumInput
        | "NUM_DISPLAY" -> Some NumDisplay
        | "TEXT" -> Some Text
        | _ -> None

    /// 비트(M/P) 주소를 쓰는 종류
    let isBit kind = kind = Switch || kind = Lamp || kind = SwitchLamp

    /// D WORD 주소를 쓰는 종류
    let isWord kind = kind = NumInput || kind = NumDisplay

    /// 조작 버튼이 있어서 '스위치 동작' 을 쓰는 종류
    let hasAction kind = kind = Switch || kind = SwitchLamp


/// 스위치 동작. 라벨 문자열은 기존 프로젝트 XML과 동일하게 유지한다.
type SwitchAction =
    | Toggle
    | On
    | Off
    | Momentary

    member this.Code =
        match this with
        | Toggle -> "토글"
        | On -> "ON"
        | Off -> "OFF"
        | Momentary -> "순간"

[<RequireQualifiedAccess>]
module SwitchAction =

    let all = [ Toggle; On; Off; Momentary ]

    let codes = all |> List.map (fun a -> a.Code)

    let parse (s: string) =
        match (if isNull s then "" else s.Trim()) with
        | "ON" -> On
        | "OFF" -> Off
        | "순간" -> Momentary
        // 예전 파일의 'ON/OFF'(ON·OFF 두 버튼)는 없앴다. 한 버튼으로 양쪽을 다루는 토글로 읽는다.
        | "ON/OFF" -> Toggle
        | _ -> Toggle


/// 화면 요소 한 개. (기존 HmiItem 과 1:1 대응)
type HmiItem =
    { Id: string
      Enabled: bool
      Kind: ItemKind
      Name: string
      Device: string
      MonitorDevice: string
      Action: SwitchAction
      Min: int
      Max: int
      X: int
      Y: int
      Width: int
      Height: int }


/// 프로젝트 한 개. (기존 HmiProject 와 1:1 대응)
/// ScreenWidth/ScreenHeight 는 v6 에 없던 항목이라 없으면 기본값을 쓴다.
/// (v6 의 XmlSerializer 는 모르는 요소를 건너뛰므로 이 파일을 v6 에서도 그대로 열 수 있다.)
type HmiProject =
    { PlcIp: string
      Port: int
      CycleMs: int
      Items: HmiItem list
      /// 배치할 수 있는 도면 크기 (스크롤 영역)
      ScreenWidth: int
      ScreenHeight: int }


[<RequireQualifiedAccess>]
module Limits =
    let minWidth = 80
    let minHeight = 55
    let minScreenWidth = 640
    let minScreenHeight = 480
    let maxScreenSize = 20000
    let defaultScreenWidth = 1600
    let defaultScreenHeight = 1000
    let minCycleMs = 100
    let maxCycleMs = 5000
    let defaultCycleMs = 300
    let defaultPort = 2004
    let defaultIp = "192.168.1.120"
    /// 한 프레임에서 읽을 수 있는 WORD 최대 개수 (XGT 사양)
    let maxWordsPerFrame = 16


[<RequireQualifiedAccess>]
module Item =

    let newId () = Guid.NewGuid().ToString("N")

    let empty =
        { Id = newId ()
          Enabled = true
          Kind = Switch
          Name = "새 스위치"
          Device = "M1000"
          MonitorDevice = ""
          Action = Toggle
          Min = 0
          Max = 65535
          X = 20
          Y = 20
          Width = 180
          Height = 100 }

    /// 새 요소의 종류별 기본값 (원본 AddEditorItem 과 동일)
    let create kind =
        match kind with
        | Switch -> { empty with Id = newId (); Kind = Switch; Name = "새 스위치"; Device = "M1000"; Action = Toggle }
        | Lamp -> { empty with Id = newId (); Kind = Lamp; Name = "새 램프"; Device = "P00000" }
        | SwitchLamp ->
            { empty with
                Id = newId ()
                Kind = SwitchLamp
                Name = "새 스위치/램프"
                Device = "M1000"
                Action = Toggle
                Width = 190
                Height = 150 }
        | MasterSwitch ->
            { empty with
                Id = newId ()
                Kind = MasterSwitch
                Name = "새 통합 스위치"
                Device = ""
                Width = 280
                Height = 200 }
        | NumInput ->
            { empty with
                Id = newId ()
                Kind = NumInput
                Name = "새 숫자입력"
                Device = "D200"
                Width = 230
                Height = 120 }
        | NumDisplay ->
            { empty with
                Id = newId ()
                Kind = NumDisplay
                Name = "새 숫자표시"
                Device = "D100"
                Width = 210
                Height = 110 }
        | Text ->
            { empty with
                Id = newId ()
                Kind = Text
                Name = "새 텍스트"
                Device = ""
                Width = 300
                Height = 70 }

    let clone (newId': bool) (src: HmiItem) =
        if newId' then { src with Id = newId () } else src

    /// 편집 값 보정 (원본 ApplyEditorToProject 의 clamp 규칙)
    let normalize (h: HmiItem) =
        let mn, mx = if h.Max < h.Min then h.Max, h.Min else h.Min, h.Max
        { h with
            Device = (if isNull h.Device then "" else h.Device.Trim().ToUpperInvariant())
            MonitorDevice = (if isNull h.MonitorDevice then "" else h.MonitorDevice.Trim().ToUpperInvariant())
            Name = (if isNull h.Name then "" else h.Name)
            Min = mn
            Max = mx
            X = max 0 h.X
            Y = max 0 h.Y
            Width = max Limits.minWidth h.Width
            Height = max Limits.minHeight h.Height }

    /// 원본 ValidateItem 과 동일한 규칙 / 동일한 메시지
    let validate (h: HmiItem) : Result<unit, string> =
        let blank (s: string) = String.IsNullOrWhiteSpace s
        match h.Kind with
        // 통합 스위치는 제 주소가 없고 대상 요소의 주소를 쓴다.
        | Text
        | MasterSwitch -> Ok()
        | kind ->
            if blank h.Device then
                Error(sprintf "'%s'의 디바이스가 비어 있습니다." h.Name)
            else
                let c = Char.ToUpperInvariant h.Device.[0]
                match kind with
                | Switch
                | Lamp
                | SwitchLamp ->
                    if c <> 'M' && c <> 'P' then
                        Error(sprintf "'%s'은 M 또는 P 비트 주소를 사용해야 합니다." h.Name)
                    elif not (blank h.MonitorDevice) then
                        let m = Char.ToUpperInvariant h.MonitorDevice.[0]
                        if m <> 'M' && m <> 'P' then
                            Error(sprintf "상태확인 디바이스는 M/P 비트만 가능합니다: %s" h.Name)
                        else Ok()
                    else Ok()
                | NumInput
                | NumDisplay ->
                    if c <> 'D' then Error(sprintf "'%s'은 D WORD 주소를 사용해야 합니다." h.Name)
                    elif h.Min < -32768 || h.Max > 65535 then
                        Error(sprintf "'%s'의 WORD 범위는 -32768 ~ 65535 안에서 설정하십시오." h.Name)
                    else Ok()
                | Text
                | MasterSwitch -> Ok()


[<RequireQualifiedAccess>]
module Project =

    let empty =
        { PlcIp = Limits.defaultIp
          Port = Limits.defaultPort
          CycleMs = Limits.defaultCycleMs
          Items = []
          ScreenWidth = Limits.defaultScreenWidth
          ScreenHeight = Limits.defaultScreenHeight }

    /// 요소가 도면 밖으로 나가 있으면 도면을 그만큼 넓힌다.
    let fitScreen (p: HmiProject) =
        let needWidth = p.Items |> List.fold (fun acc h -> max acc (h.X + h.Width + 40)) Limits.minScreenWidth
        let needHeight = p.Items |> List.fold (fun acc h -> max acc (h.Y + h.Height + 40)) Limits.minScreenHeight
        { p with
            ScreenWidth = min Limits.maxScreenSize (max p.ScreenWidth needWidth)
            ScreenHeight = min Limits.maxScreenSize (max p.ScreenHeight needHeight) }

    /// 원본 CreateDefaultProject 와 동일한 r004 기본 예제
    let createDefault () =
        let switches =
            [ "M01008", "P00120", "sys tr in enable c"
              "M01009", "P00121", "job start"
              "M01010", "P00122", "job exit"
              "M01011", "P00123", "job pause"
              "M01012", "P00124", "job restart"
              "M01013", "P00125", "alarm reset"
              "M01014", "P00126", "servo on/off"
              "M01015", "P00127", "P00127 제어"
              "M01016", "P00128", "P00128 제어"
              "M01006", "P00130", "P00130 제어"
              "M01007", "P00131", "P00131 제어"
              "M00510", "P00138", "P00138 제어"
              "M00501", "P00139", "P00139 제어"
              "M01002", "P0013A", "P0013A 제어"
              "M01003", "P0013B", "P0013B 제어"
              "M01005", "P0013C", "외부도어 닫힘"
              "M01004", "P0013D", "외부도어 열림"
              "M01000", "P0013E", "P0013E 제어"
              "M01001", "P0013F", "P0013F 제어" ]

        let swItems =
            switches
            |> List.mapi (fun i (device, monitor, name) ->
                let col = i % 5
                let row = i / 5
                { Item.empty with
                    Id = Item.newId ()
                    Kind = Switch
                    Name = name
                    Device = device
                    MonitorDevice = monitor
                    Action = Toggle
                    X = 18 + col * 220
                    Y = 18 + row * 118
                    Width = 205
                    Height = 105 })

        let numInput =
            { Item.empty with
                Id = Item.newId ()
                Kind = NumInput
                Name = "D200 설정값"
                Device = "D200"
                Min = -32768
                Max = 65535
                X = 18
                Y = 500
                Width = 250
                Height = 125 }

        let numDisplay =
            { Item.empty with
                Id = Item.newId ()
                Kind = NumDisplay
                Name = "D100 현재값"
                Device = "D100"
                X = 285
                Y = 500
                Width = 220
                Height = 125 }

        let text =
            { Item.empty with
                Id = Item.newId ()
                Kind = Text
                Name = "※ PLC 래더: MOV D200 D100 으로 변경 후 D200 설정 사용"
                Device = ""
                X = 525
                Y = 520
                Width = 520
                Height = 70 }

        { PlcIp = Limits.defaultIp
          Port = Limits.defaultPort
          CycleMs = Limits.defaultCycleMs
          Items = swItems @ [ numInput; numDisplay; text ]
          ScreenWidth = Limits.defaultScreenWidth
          ScreenHeight = Limits.defaultScreenHeight }

    /// 폴링 주기에 읽어야 할 비트 / WORD 주소 목록
    let scanAddresses (items: HmiItem seq) =
        let bits = ResizeArray<string>()
        let words = ResizeArray<string>()

        let addUnique (list: ResizeArray<string>) (value: string) =
            if not (String.IsNullOrWhiteSpace value) then
                if not (list |> Seq.exists (fun s -> String.Equals(s, value, StringComparison.OrdinalIgnoreCase))) then
                    list.Add value

        for h in items do
            if h.Enabled then
                if ItemKind.isBit h.Kind then
                    addUnique bits h.Device
                    addUnique bits h.MonitorDevice
                elif ItemKind.isWord h.Kind then
                    addUnique words h.Device

        List.ofSeq bits, List.ofSeq words

    /// '스위치 수량 추가'용 다음 M 주소 (원본 FindNextMAddress)
    let nextMAddress (items: HmiItem seq) =
        let mutable maxV = 999
        for h in items do
            let d = if isNull h.Device then "" else h.Device.Trim().ToUpperInvariant()
            if d.StartsWith "M" then
                match Int32.TryParse(d.Substring 1) with
                | true, v when v > maxV -> maxV <- v
                | _ -> ()
        maxV + 1

    /// 새 요소 자동 배치 위치 (원본 FindNextFreeEditorPosition)
    let nextFreePosition (index: int) =
        let col = index % 5
        let row = index / 5
        20 + col * 220, 20 + row * 118
