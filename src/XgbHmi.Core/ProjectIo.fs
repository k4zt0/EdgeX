namespace XgbHmi.Core

open System
open System.IO
open System.Xml
open System.Xml.Linq

/// 기존 v6(WinForms/XmlSerializer)가 쓰던 r004_hmi_project.xml 형식을 그대로 읽고 쓴다.
/// 어떤 OS에서 저장하든 같은 파일을 서로 열 수 있다.
[<RequireQualifiedAccess>]
module ProjectIo =

    let private el (parent: XElement) (name: string) =
        match parent.Element(XName.Get name) with
        | null -> None
        | e -> Some e

    let private str (parent: XElement) (name: string) (fallback: string) =
        el parent name |> Option.map (fun e -> e.Value) |> Option.defaultValue fallback

    let private int' (parent: XElement) (name: string) (fallback: int) =
        match el parent name with
        | Some e ->
            match Int32.TryParse(e.Value.Trim(), Globalization.NumberStyles.Integer, Globalization.CultureInfo.InvariantCulture) with
            | true, v -> v
            | _ -> fallback
        | None -> fallback

    let private bool' (parent: XElement) (name: string) (fallback: bool) =
        match el parent name with
        | Some e ->
            match Boolean.TryParse(e.Value.Trim()) with
            | true, v -> v
            | _ -> fallback
        | None -> fallback

    let private parseItem (e: XElement) : HmiItem =
        let kind =
            str e "Type" "SWITCH"
            |> ItemKind.tryParse
            |> Option.defaultValue Switch

        { Id =
            let id = str e "Id" ""
            if String.IsNullOrWhiteSpace id then Item.newId () else id
          Enabled = bool' e "Enabled" true
          // v6 파일에는 없는 항목이다. 비어 있으면 첫 번째 PLC 를 쓴다.
          PlcId = (str e "PlcId" "").Trim()
          // 이 항목이 없는 예전 파일은 지금까지처럼 전부 보이게 읽는다.
          Visible = bool' e "Visible" true
          Kind = kind
          Name = str e "Name" ""
          Device = (str e "Device" "").Trim().ToUpperInvariant()
          MonitorDevice = (str e "MonitorDevice" "").Trim().ToUpperInvariant()
          Action = SwitchAction.parse (str e "Action" "토글")
          Min = int' e "Min" 0
          Max = int' e "Max" 65535
          X = int' e "X" 20
          Y = int' e "Y" 20
          Width = int' e "Width" 180
          Height = int' e "Height" 100 }
        |> Item.normalize

    let private itemElement (h: HmiItem) =
        XElement(
            XName.Get "HmiItem",
            XElement(XName.Get "Id", h.Id),
            XElement(XName.Get "Enabled", (if h.Enabled then "true" else "false")),
            XElement(XName.Get "Visible", (if h.Visible then "true" else "false")),
            XElement(XName.Get "PlcId", h.PlcId),
            XElement(XName.Get "Type", h.Kind.Code),
            XElement(XName.Get "Name", h.Name),
            XElement(XName.Get "Device", h.Device),
            XElement(XName.Get "MonitorDevice", h.MonitorDevice),
            XElement(XName.Get "Action", h.Action.Code),
            XElement(XName.Get "Min", string h.Min),
            XElement(XName.Get "Max", string h.Max),
            XElement(XName.Get "X", string h.X),
            XElement(XName.Get "Y", string h.Y),
            XElement(XName.Get "Width", string h.Width),
            XElement(XName.Get "Height", string h.Height)
        )

    // ---------- PLC 목록 ----------
    // v6 XmlSerializer 는 모르는 요소를 건너뛰므로 <Plcs> 를 붙여도 예전 프로그램에서 그대로 열린다.
    // (v6 는 <PlcIp>/<Port>/<CycleMs> 만 보고 첫 이더넷 PLC 한 대로 동작한다)

    let private parsePlc (e: XElement) : PlcLink =
        let kind =
            str e "Kind" "ETHERNET"
            |> PlcLinkKind.tryParse
            |> Option.defaultValue LinkEthernet
        { Id = (str e "Id" "").Trim()
          Name = str e "Name" ""
          Kind = kind
          Enabled = bool' e "Enabled" true
          Ip = (str e "Ip" Limits.defaultIp).Trim()
          Port = int' e "Port" Limits.defaultPort
          SerialPort = (str e "SerialPort" "").Trim()
          Baud = int' e "Baud" Limits.defaultBaud
          DataBits = int' e "DataBits" Limits.defaultDataBits
          Parity = str e "Parity" Limits.defaultParity
          StopBits = int' e "StopBits" Limits.defaultStopBits
          Station = int' e "Station" Limits.defaultStation
          CycleMs = int' e "CycleMs" Limits.defaultCycleMs }
        |> PlcLink.normalize

    let private plcElement (l: PlcLink) =
        XElement(
            XName.Get "PlcLink",
            XElement(XName.Get "Id", l.Id),
            XElement(XName.Get "Name", l.Name),
            XElement(XName.Get "Kind", l.Kind.Code),
            XElement(XName.Get "Enabled", (if l.Enabled then "true" else "false")),
            XElement(XName.Get "Ip", l.Ip),
            XElement(XName.Get "Port", string l.Port),
            XElement(XName.Get "SerialPort", l.SerialPort),
            XElement(XName.Get "Baud", string l.Baud),
            XElement(XName.Get "DataBits", string l.DataBits),
            XElement(XName.Get "Parity", l.Parity),
            XElement(XName.Get "StopBits", string l.StopBits),
            XElement(XName.Get "Station", string l.Station),
            XElement(XName.Get "CycleMs", string l.CycleMs)
        )

    let private parsePlcs (root: XElement) : PlcLink list =
        match el root "Plcs" with
        | None -> []
        | Some e -> e.Elements(XName.Get "PlcLink") |> Seq.map parsePlc |> List.ofSeq

    let private plcsElement (plcs: PlcLink list) =
        let list = XElement(XName.Get "Plcs")
        for l in plcs do
            list.Add(plcElement l)
        list

    // ---------- 터치스크린(HMI) 화면 ----------
    // v6 XmlSerializer 는 모르는 요소를 건너뛰므로 <Hmi> 를 붙여도 예전 프로그램에서 그대로 열린다.

    let private parsePart (e: XElement) : HmiPart =
        let kind =
            str e "Type" "BUTTON"
            |> HmiPartKind.tryParse
            |> Option.defaultValue PartButton

        { Id =
            let id = str e "Id" ""
            if String.IsNullOrWhiteSpace id then HmiPart.newId () else id
          Kind = kind
          TargetId = str e "TargetId" ""
          SubTargetId = str e "SubTargetId" ""
          Text = str e "Text" ""
          OnText = str e "OnText" ""
          OffText = str e "OffText" ""
          Unit = str e "Unit" ""
          X = int' e "X" 40
          Y = int' e "Y" 40
          Width = int' e "Width" 180
          Height = int' e "Height" 90
          Shape = str e "Shape" "RECT"
          OffColor = str e "OffColor" ""
          OnColor = str e "OnColor" ""
          TextColor = str e "TextColor" ""
          BorderColor = str e "BorderColor" ""
          FontSize = int' e "FontSize" 18
          Corner = int' e "Corner" 8
          Align = str e "Align" "CENTER"
          Step = int' e "Step" 0
          Action = str e "Action" ""
          Count = int' e "Count" 8
          Decimals = int' e "Decimals" 0
          WriteValue = int' e "WriteValue" 0
          Options = str e "Options" ""
          Vertical = bool' e "Vertical" false
          ScaleMin = int' e "ScaleMin" 0
          ScaleMax = int' e "ScaleMax" 0
          ThenOnId = str e "ThenOnId" ""
          Group = str e "Group" "" }
        |> HmiPart.normalize

    let private partElement (p: HmiPart) =
        XElement(
            XName.Get "HmiPart",
            XElement(XName.Get "Id", p.Id),
            XElement(XName.Get "Type", p.Kind.Code),
            XElement(XName.Get "TargetId", p.TargetId),
            XElement(XName.Get "SubTargetId", p.SubTargetId),
            XElement(XName.Get "Text", p.Text),
            XElement(XName.Get "OnText", p.OnText),
            XElement(XName.Get "OffText", p.OffText),
            XElement(XName.Get "Unit", p.Unit),
            XElement(XName.Get "X", string p.X),
            XElement(XName.Get "Y", string p.Y),
            XElement(XName.Get "Width", string p.Width),
            XElement(XName.Get "Height", string p.Height),
            XElement(XName.Get "Shape", p.Shape),
            XElement(XName.Get "OffColor", p.OffColor),
            XElement(XName.Get "OnColor", p.OnColor),
            XElement(XName.Get "TextColor", p.TextColor),
            XElement(XName.Get "BorderColor", p.BorderColor),
            XElement(XName.Get "FontSize", string p.FontSize),
            XElement(XName.Get "Corner", string p.Corner),
            XElement(XName.Get "Align", p.Align),
            XElement(XName.Get "Step", string p.Step),
            XElement(XName.Get "Action", p.Action),
            XElement(XName.Get "Count", string p.Count),
            XElement(XName.Get "Decimals", string p.Decimals),
            XElement(XName.Get "WriteValue", string p.WriteValue),
            XElement(XName.Get "Options", p.Options),
            XElement(XName.Get "Vertical", (if p.Vertical then "true" else "false")),
            XElement(XName.Get "ScaleMin", string p.ScaleMin),
            XElement(XName.Get "ScaleMax", string p.ScaleMax),
            XElement(XName.Get "ThenOnId", p.ThenOnId),
            XElement(XName.Get "Group", p.Group)
        )

    let private parseHmi (root: XElement) : HmiScreen =
        match el root "Hmi" with
        | None -> HmiScreen.empty
        | Some e ->
            let parts =
                match el e "Parts" with
                | Some list -> list.Elements(XName.Get "HmiPart") |> Seq.map parsePart |> List.ofSeq
                | None -> []
            { Width = int' e "Width" HmiLimits.defaultWidth
              Height = int' e "Height" HmiLimits.defaultHeight
              Background = str e "Background" ""
              Parts = parts }
            |> HmiScreen.normalize

    let private hmiElement (s: HmiScreen) =
        let parts = XElement(XName.Get "Parts")
        for p in s.Parts do
            parts.Add(partElement p)
        XElement(
            XName.Get "Hmi",
            XElement(XName.Get "Width", string s.Width),
            XElement(XName.Get "Height", string s.Height),
            XElement(XName.Get "Background", s.Background),
            parts
        )

    let load (path: string) : HmiProject =
        let doc = XDocument.Load(path, LoadOptions.None)
        let root = doc.Root
        if isNull root then failwith "프로젝트 XML의 루트 요소가 없습니다."

        let items =
            match el root "Items" with
            | Some items -> items.Elements(XName.Get "HmiItem") |> Seq.map parseItem |> List.ofSeq
            | None -> []

        let port = int' root "Port" Limits.defaultPort
        let cycle = int' root "CycleMs" Limits.defaultCycleMs

        let screenWidth = int' root "ScreenWidth" Limits.defaultScreenWidth
        let screenHeight = int' root "ScreenHeight" Limits.defaultScreenHeight

        { PlcIp =
            let ip = str root "PlcIp" Limits.defaultIp
            if String.IsNullOrWhiteSpace ip then Limits.defaultIp else ip.Trim()
          Port = if port < 1 || port > 65535 then Limits.defaultPort else port
          CycleMs = if cycle < Limits.minCycleMs then Limits.defaultCycleMs else min Limits.maxCycleMs cycle
          Plcs = parsePlcs root
          Items = items
          ScreenWidth = max Limits.minScreenWidth (min Limits.maxScreenSize screenWidth)
          ScreenHeight = max Limits.minScreenHeight (min Limits.maxScreenSize screenHeight)
          Hmi = parseHmi root }
        // <Plcs> 가 없는 v6 파일은 PlcIp/Port/CycleMs 로 이더넷 한 대를 만든다.
        |> Project.normalizeLinks
        |> Project.fitScreen

    let save (path: string) (p: HmiProject) =
        // 저장하기 전에 PLC 목록을 정리한다. v6 가 읽는 PlcIp/Port/CycleMs 도 여기서 맞춰진다.
        let p = Project.normalizeLinks p
        let items = XElement(XName.Get "Items")
        for h in p.Items do
            items.Add(itemElement h)

        let root = XElement(XName.Get "HmiProject")
        root.Add(XElement(XName.Get "PlcIp", p.PlcIp))
        root.Add(XElement(XName.Get "Port", string p.Port))
        root.Add(XElement(XName.Get "CycleMs", string p.CycleMs))
        root.Add items
        // v6 는 모르는 요소를 건너뛰므로 Items 뒤에 붙여도 호환된다.
        root.Add(XElement(XName.Get "ScreenWidth", string p.ScreenWidth))
        root.Add(XElement(XName.Get "ScreenHeight", string p.ScreenHeight))
        root.Add(plcsElement p.Plcs)
        root.Add(hmiElement p.Hmi)

        let doc = XDocument()
        doc.Declaration <- XDeclaration("1.0", "utf-8", null)
        doc.Add root

        let dir = Path.GetDirectoryName(Path.GetFullPath path)
        if not (String.IsNullOrEmpty dir) && not (Directory.Exists dir) then
            Directory.CreateDirectory dir |> ignore

        let settings = XmlWriterSettings(Indent = true, IndentChars = "  ", Encoding = Text.UTF8Encoding false)
        use writer = XmlWriter.Create(path, settings)
        doc.Save writer

    let loadOrDefault (path: string) : HmiProject =
        try
            if File.Exists path then load path
            else
                let p = Project.createDefault ()
                (try save path p with _ -> ())
                p
        with _ ->
            Project.createDefault ()

    /// 사용자 데이터 폴더 (Windows: %AppData%, macOS: ~/Library/Application Support, Linux: ~/.config)
    let userDataDirectory () =
        let root = Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData)
        let root = if String.IsNullOrWhiteSpace root then Path.GetTempPath() else root
        Path.Combine(root, "XgbHmiDesigner")

    /// 기본 프로젝트 경로를 OS 중립적으로 찾는다.
    /// 1) XGBHMI_PROJECT 환경변수  2) 현재 작업 폴더  3) 실행 파일 폴더  4) 사용자 데이터 폴더
    let defaultProjectPath () =
        let fileName = "r004_hmi_project.xml"
        let env = Environment.GetEnvironmentVariable "XGBHMI_PROJECT"
        if not (String.IsNullOrWhiteSpace env) then
            Path.GetFullPath env
        else
            let cwd = Path.Combine(Directory.GetCurrentDirectory(), fileName)
            let baseDir = Path.Combine(AppContext.BaseDirectory, fileName)
            if File.Exists cwd then Path.GetFullPath cwd
            elif File.Exists baseDir then Path.GetFullPath baseDir
            else
                let userDir = userDataDirectory ()
                let userPath = Path.Combine(userDir, fileName)
                if File.Exists userPath then userPath
                else
                    // 쓰기 가능한 곳을 고른다: 현재 작업 폴더에 쓸 수 있으면 그대로, 아니면 사용자 폴더.
                    try
                        let probe = Path.Combine(Directory.GetCurrentDirectory(), ".xgbhmi_write_test")
                        File.WriteAllText(probe, "")
                        File.Delete probe
                        Path.GetFullPath cwd
                    with _ ->
                        Directory.CreateDirectory userDir |> ignore
                        userPath
