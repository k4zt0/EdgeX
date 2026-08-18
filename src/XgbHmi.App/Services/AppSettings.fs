namespace XgbHmi.App.Services

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

/// 사용자별 UI 설정. 프로젝트 XML과 분리해서 사용자 폴더에 둔다.
type AppSettings =
    { [<JsonPropertyName("theme")>] Theme: string
      [<JsonPropertyName("language")>] Language: string
      [<JsonPropertyName("lastProject")>] LastProject: string
      [<JsonPropertyName("showGrid")>] ShowGrid: bool
      [<JsonPropertyName("snapToGrid")>] SnapToGrid: bool
      [<JsonPropertyName("zoom")>] Zoom: float
      [<JsonPropertyName("windowWidth")>] WindowWidth: float
      [<JsonPropertyName("windowHeight")>] WindowHeight: float }

[<RequireQualifiedAccess>]
module AppSettings =

    let defaults =
        { Theme = "dark"
          Language = ""
          LastProject = ""
          ShowGrid = true
          SnapToGrid = false
          Zoom = 1.0
          WindowWidth = 1500.0
          WindowHeight = 950.0 }

    let private path () =
        let dir = XgbHmi.Core.ProjectIo.userDataDirectory ()
        Path.Combine(dir, "settings.json")

    let load () =
        try
            let p = path ()
            if File.Exists p then
                let json = File.ReadAllText p
                let opts = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
                match JsonSerializer.Deserialize<AppSettings>(json, opts) with
                | s when not (obj.ReferenceEquals(s, null)) ->
                    { s with
                        Theme = (if String.IsNullOrWhiteSpace s.Theme then defaults.Theme else s.Theme)
                        Zoom = (if s.Zoom < 0.4 || s.Zoom > 3.0 then 1.0 else s.Zoom)
                        WindowWidth = (if s.WindowWidth < 900.0 then defaults.WindowWidth else s.WindowWidth)
                        WindowHeight = (if s.WindowHeight < 600.0 then defaults.WindowHeight else s.WindowHeight) }
                | _ -> defaults
            else defaults
        with _ -> defaults

    let save (s: AppSettings) =
        try
            let p = path ()
            Directory.CreateDirectory(Path.GetDirectoryName p) |> ignore
            let opts = JsonSerializerOptions(WriteIndented = true)
            File.WriteAllText(p, JsonSerializer.Serialize(s, opts))
        with _ -> ()
