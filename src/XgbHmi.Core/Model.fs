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


/// PLC 한 대에 붙는 방법. 이더넷(FEnet) 과 직렬(Cnet) 을 함께 쓸 수 있다.
type PlcLinkKind =
    /// 이더넷 FEnet (TCP)
    | LinkEthernet
    /// RS-232C Cnet — 1:1 직렬
    | LinkRs232
    /// RS-485 Cnet — 한 회선에 여러 대를 국번으로 구분해서 붙인다
    | LinkRs485

    member this.Code =
        match this with
        | LinkEthernet -> "ETHERNET"
        | LinkRs232 -> "RS232"
        | LinkRs485 -> "RS485"

    /// 직렬 회선(Cnet) 인지. 이 둘은 프레임이 같고 회선만 다르다.
    member this.IsSerial =
        match this with
        | LinkEthernet -> false
        | LinkRs232
        | LinkRs485 -> true


[<RequireQualifiedAccess>]
module PlcLinkKind =

    let all = [ LinkEthernet; LinkRs232; LinkRs485 ]

    let codes = all |> List.map (fun k -> k.Code)

    let tryParse (s: string) =
        match (if isNull s then "" else s.Trim().ToUpperInvariant()) with
        | "ETHERNET"
        | "FENET"
        | "TCP" -> Some LinkEthernet
        | "RS232"
        | "RS-232"
        | "RS232C"
        | "CNET" -> Some LinkRs232
        | "RS485"
        | "RS-485"
        | "RS422"
        | "RS-422" -> Some LinkRs485
        | _ -> None


/// PLC 한 대. 이더넷이면 IP/포트를, 직렬이면 포트 이름·통신 속도·국번을 쓴다.
/// 여러 대를 동시에 붙일 수 있고, 화면 요소마다 어느 PLC 를 쓸지 고른다.
type PlcLink =
    { /// 프로젝트 안에서만 쓰는 짧은 이름표 (PLC1, PLC2 ...). 화면 요소가 이 값으로 PLC 를 가리킨다.
      Id: string
      /// 사람이 보는 이름 (예: "1호기 반송")
      Name: string
      Kind: PlcLinkKind
      /// 끄면 연결하지 않는다. (설정은 그대로 남는다)
      Enabled: bool
      // ---- 이더넷 ----
      Ip: string
      Port: int
      // ---- 직렬 (RS-232C / RS-485) ----
      /// COM3 / /dev/tty.usbserial-1410 처럼 OS 가 주는 포트 이름
      SerialPort: string
      Baud: int
      DataBits: int
      /// NONE / ODD / EVEN
      Parity: string
      /// 1 또는 2
      StopBits: int
      /// Cnet 국번 0~31. RS-485 는 이 번호로 여러 대를 구분한다.
      Station: int
      /// 이 PLC 만의 폴링 주기(ms)
      CycleMs: int }


/// 화면 요소 한 개. (기존 HmiItem 과 1:1 대응)
type HmiItem =
    { Id: string
      Enabled: bool
      /// 어느 PLC 를 쓰는지 (PlcLink.Id). 비어 있으면 첫 번째 PLC 를 쓴다.
      PlcId: string
      /// 운전 화면에 카드로 띄울지. 꺼도 통합 스위치는 계속 지켜보고 폴링도 그대로 돈다.
      Visible: bool
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
    { /// v6 호환용 첫 이더넷 PLC 의 IP (Plcs 의 첫 이더넷 항목과 같게 유지한다)
      PlcIp: string
      Port: int
      CycleMs: int
      /// 붙일 PLC 목록. 이더넷·RS-232C·RS-485 를 섞어 여러 대를 함께 쓸 수 있다.
      Plcs: PlcLink list
      Items: HmiItem list
      /// 배치할 수 있는 도면 크기 (스크롤 영역)
      ScreenWidth: int
      ScreenHeight: int
      /// 터치스크린(HMI) 작화 화면. 부품은 Items 의 요소를 연결해 동작한다.
      Hmi: HmiScreen }


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
    /// 직렬(Cnet) 기본값 — XGB 내장 Cnet 출하 설정
    let defaultBaud = 9600
    let defaultDataBits = 8
    let defaultParity = "NONE"
    let defaultStopBits = 1
    let defaultStation = 0
    let maxStation = 31
    /// 고를 수 있는 통신 속도 (XGB Cnet 지원 범위)
    let bauds = [ 1200; 2400; 4800; 9600; 19200; 38400; 57600; 115200 ]
    let parities = [ "NONE"; "ODD"; "EVEN" ]
    /// 한 프레임에서 읽을 수 있는 WORD 최대 개수 (XGT 사양)
    let maxWordsPerFrame = 16


[<RequireQualifiedAccess>]
module PlcLink =

    /// 이더넷 PLC 기본값
    let ethernet (id: string) =
        { Id = id
          Name = id
          Kind = LinkEthernet
          Enabled = true
          Ip = Limits.defaultIp
          Port = Limits.defaultPort
          SerialPort = ""
          Baud = Limits.defaultBaud
          DataBits = Limits.defaultDataBits
          Parity = Limits.defaultParity
          StopBits = Limits.defaultStopBits
          Station = Limits.defaultStation
          CycleMs = Limits.defaultCycleMs }

    /// 직렬(RS-232C / RS-485) PLC 기본값
    let serial (id: string) (kind: PlcLinkKind) (station: int) =
        { ethernet id with
            Kind = kind
            Ip = ""
            Port = 0
            Station = station }

    let empty = ethernet "PLC1"

    /// 이미 있는 목록과 겹치지 않는 다음 이름표 (PLC1, PLC2 ...)
    let nextId (existing: PlcLink seq) =
        let taken =
            existing
            |> Seq.map (fun l -> (if isNull l.Id then "" else l.Id.Trim().ToUpperInvariant()))
            |> Set.ofSeq
        let mutable n = 1
        while taken.Contains("PLC" + string n) do
            n <- n + 1
        "PLC" + string n

    let normalize (l: PlcLink) =
        let id = (if isNull l.Id then "" else l.Id.Trim().ToUpperInvariant())
        { l with
            Id = (if String.IsNullOrWhiteSpace id then "PLC1" else id)
            Name = (if isNull l.Name then "" else l.Name.Trim())
            Ip = (if isNull l.Ip then "" else l.Ip.Trim())
            Port = (if l.Port < 1 || l.Port > 65535 then Limits.defaultPort else l.Port)
            SerialPort = (if isNull l.SerialPort then "" else l.SerialPort.Trim())
            Baud = (if l.Baud < 300 then Limits.defaultBaud else l.Baud)
            DataBits = (if l.DataBits <> 7 && l.DataBits <> 8 then Limits.defaultDataBits else l.DataBits)
            Parity =
                (let p = (if isNull l.Parity then "" else l.Parity.Trim().ToUpperInvariant())
                 if List.contains p Limits.parities then p else Limits.defaultParity)
            StopBits = (if l.StopBits >= 2 then 2 else 1)
            Station = max 0 (min Limits.maxStation l.Station)
            CycleMs = (if l.CycleMs < Limits.minCycleMs then Limits.defaultCycleMs else min Limits.maxCycleMs l.CycleMs) }

    /// 화면에 보여 줄 이름. 이름을 비워 두면 이름표를 쓴다.
    let label (l: PlcLink) =
        if String.IsNullOrWhiteSpace l.Name then l.Id else l.Id + " · " + l.Name

    /// 어디에 어떻게 붙는지 한 줄 요약 (툴바 / 트리 / 상태 표시줄)
    let endpoint (l: PlcLink) =
        match l.Kind with
        | LinkEthernet -> sprintf "%s:%d" l.Ip l.Port
        | LinkRs232
        | LinkRs485 ->
            sprintf
                "%s %d-%d-%s-%d  국번 %d"
                (if String.IsNullOrWhiteSpace l.SerialPort then "(포트 없음)" else l.SerialPort)
                l.Baud
                l.DataBits
                (match l.Parity with
                 | "ODD" -> "O"
                 | "EVEN" -> "E"
                 | _ -> "N")
                l.StopBits
                l.Station

    /// 연결 전 검사. 첫 오류 메시지를 돌려준다.
    let validate (l: PlcLink) : Result<unit, string> =
        let blank (s: string) = String.IsNullOrWhiteSpace s
        if blank l.Id then Error "PLC 이름표가 비어 있습니다."
        else
            match l.Kind with
            | LinkEthernet ->
                if blank l.Ip then Error(sprintf "%s: IP 주소가 비어 있습니다." (label l))
                elif l.Port < 1 || l.Port > 65535 then Error(sprintf "%s: 포트 번호가 잘못되었습니다." (label l))
                else Ok()
            | LinkRs232
            | LinkRs485 ->
                if blank l.SerialPort then Error(sprintf "%s: 직렬 포트를 고르십시오." (label l))
                elif l.Station < 0 || l.Station > Limits.maxStation then
                    Error(sprintf "%s: 국번은 0~%d 입니다." (label l) Limits.maxStation)
                else Ok()


[<RequireQualifiedAccess>]
module Item =

    let newId () = Guid.NewGuid().ToString("N")

    let empty =
        { Id = newId ()
          Enabled = true
          PlcId = ""
          Visible = true
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
            // 대상 고르기 + 목록 + 조작 + 전체 종료가 한 장에 들어가야 해서 넉넉하게 잡는다.
            { empty with
                Id = newId ()
                Kind = MasterSwitch
                Name = "새 통합 스위치"
                Device = ""
                Width = 320
                Height = 380 }
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
            PlcId = (if isNull h.PlcId then "" else h.PlcId.Trim())
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
          Plcs = [ PlcLink.empty ]
          Items = []
          ScreenWidth = Limits.defaultScreenWidth
          ScreenHeight = Limits.defaultScreenHeight
          Hmi = HmiScreen.empty }

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
          Plcs = [ PlcLink.empty ]
          Items = swItems @ [ numInput; numDisplay; text ]
          ScreenWidth = Limits.defaultScreenWidth
          ScreenHeight = Limits.defaultScreenHeight
          Hmi = HmiScreen.empty }

    /// PLC 목록을 쓸 수 있는 상태로 만든다.
    /// 비어 있으면 v6 파일처럼 PlcIp/Port/CycleMs 로 이더넷 한 대를 만들고,
    /// 이름표가 겹치면 뒤에 온 쪽에 새 이름표를 준다.
    let normalizePlcs (p: HmiProject) =
        let source =
            if p.Plcs.IsEmpty then
                [ { PlcLink.empty with
                      Ip = (if String.IsNullOrWhiteSpace p.PlcIp then Limits.defaultIp else p.PlcIp.Trim())
                      Port = p.Port
                      CycleMs = p.CycleMs } ]
            else p.Plcs
        let result = ResizeArray<PlcLink>()
        for link in source do
            let normalized = PlcLink.normalize link
            let unique =
                if result |> Seq.exists (fun l -> String.Equals(l.Id, normalized.Id, StringComparison.OrdinalIgnoreCase)) then
                    { normalized with Id = PlcLink.nextId result }
                else normalized
            result.Add unique
        List.ofSeq result

    /// 요소가 가리키는 PLC. 이름표가 비었거나 없어진 PLC 를 가리키면 첫 번째 PLC 를 쓴다.
    let resolvePlcId (plcs: PlcLink list) (plcId: string) =
        let id = if isNull plcId then "" else plcId.Trim()
        match plcs |> List.tryFind (fun l -> String.Equals(l.Id, id, StringComparison.OrdinalIgnoreCase)) with
        | Some l -> l.Id
        | None ->
            match plcs with
            | first :: _ -> first.Id
            | [] -> ""

    /// PLC 목록을 정리하고, 요소의 PLC 이름표도 실제로 있는 PLC 로 맞춘다.
    let normalizeLinks (p: HmiProject) =
        let plcs = normalizePlcs p
        let items = p.Items |> List.map (fun h -> { h with PlcId = resolvePlcId plcs h.PlcId })
        // v6(WinForms) 는 PlcIp/Port/CycleMs 만 읽으므로 첫 이더넷 PLC 를 그 자리에 그대로 둔다.
        let firstEthernet = plcs |> List.tryFind (fun l -> l.Kind = LinkEthernet)
        { p with
            Plcs = plcs
            Items = items
            PlcIp = (match firstEthernet with Some l -> l.Ip | None -> p.PlcIp)
            Port = (match firstEthernet with Some l -> l.Port | None -> p.Port)
            CycleMs = (match plcs with first :: _ -> first.CycleMs | [] -> p.CycleMs) }

    /// PLC 목록 전체 검사. 첫 오류 메시지를 돌려준다.
    let validatePlcs (plcs: PlcLink list) : Result<unit, string> =
        let enabled = plcs |> List.filter (fun l -> l.Enabled)
        if enabled.IsEmpty then Error "연결할 PLC 가 없습니다. 적어도 한 대는 '사용' 으로 두십시오."
        else
            let firstError = enabled |> List.tryPick (fun l -> match PlcLink.validate l with Error m -> Some m | Ok() -> None)
            match firstError with
            | Some m -> Error m
            | None ->
                // 같은 직렬 회선에 같은 국번이 두 대 있으면 응답을 구분할 수 없다.
                let serials =
                    enabled
                    |> List.filter (fun l -> l.Kind.IsSerial)
                    |> List.map (fun l -> l.SerialPort.Trim().ToUpperInvariant(), l.Station, l)
                let duplicate =
                    serials
                    |> List.tryPick (fun (port, station, l) ->
                        if serials |> List.filter (fun (p2, s2, _) -> p2 = port && s2 = station) |> List.length > 1 then Some l
                        else None)
                match duplicate with
                | Some l -> Error(sprintf "%s: 같은 회선(%s)에 국번 %d 가 두 번 있습니다." (PlcLink.label l) l.SerialPort l.Station)
                | None ->
                    // 같은 회선을 쓰면 통신 속도·패리티·정지 비트가 같아야 한다.
                    let conflict =
                        serials
                        |> List.tryPick (fun (port, _, l) ->
                            serials
                            |> List.tryPick (fun (p2, _, other) ->
                                if p2 = port
                                   && (other.Baud <> l.Baud || other.Parity <> l.Parity || other.StopBits <> l.StopBits || other.DataBits <> l.DataBits)
                                then Some(l, other)
                                else None))
                    match conflict with
                    | Some(l, other) ->
                        Error(
                            sprintf
                                "%s 와 %s 는 같은 회선(%s)을 쓰므로 통신 속도·패리티·정지 비트를 같게 맞추십시오."
                                (PlcLink.label l)
                                (PlcLink.label other)
                                l.SerialPort
                        )
                    | None -> Ok()

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

    /// PLC 별 폴링 목록. 여러 대를 붙였을 때 각 회선이 제 주소만 읽게 나눈다.
    let scanAddressesByPlc (plcs: PlcLink list) (items: HmiItem seq) =
        let items = List.ofSeq items
        plcs
        |> List.filter (fun l -> l.Enabled)
        |> List.map (fun link ->
            let mine =
                items
                |> List.filter (fun h -> String.Equals(resolvePlcId plcs h.PlcId, link.Id, StringComparison.OrdinalIgnoreCase))
            let bits, words = scanAddresses mine
            link.Id, bits, words)

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
