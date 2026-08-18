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
          Items = items
          ScreenWidth = max Limits.minScreenWidth (min Limits.maxScreenSize screenWidth)
          ScreenHeight = max Limits.minScreenHeight (min Limits.maxScreenSize screenHeight) }
        |> Project.fitScreen

    let save (path: string) (p: HmiProject) =
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
