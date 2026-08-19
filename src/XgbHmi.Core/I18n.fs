namespace XgbHmi.Core

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.Json

/// 지원 언어 한 개.
type Language =
    { Code: string
      /// 해당 언어 사용자가 보는 이름
      Native: string
      /// 영어 이름 (메뉴 보조 표기)
      English: string
      /// 오른쪽에서 왼쪽으로 쓰는 언어 (아랍어 등)
      Rtl: bool }

/// 20개 언어 UI 문자열 카탈로그.
/// Assets/i18n.json 을 어셈블리에 포함해 배포하므로 OS/설치 위치와 무관하게 동작한다.
[<RequireQualifiedAccess>]
module I18n =

    let languages =
        [ { Code = "ko"; Native = "한국어"; English = "Korean"; Rtl = false }
          { Code = "en"; Native = "English"; English = "English"; Rtl = false }
          { Code = "ja"; Native = "日本語"; English = "Japanese"; Rtl = false }
          { Code = "zh-Hans"; Native = "简体中文"; English = "Chinese (Simplified)"; Rtl = false }
          { Code = "zh-Hant"; Native = "繁體中文"; English = "Chinese (Traditional)"; Rtl = false }
          { Code = "de"; Native = "Deutsch"; English = "German"; Rtl = false }
          { Code = "fr"; Native = "Français"; English = "French"; Rtl = false }
          { Code = "es"; Native = "Español"; English = "Spanish"; Rtl = false }
          { Code = "pt"; Native = "Português"; English = "Portuguese"; Rtl = false }
          { Code = "it"; Native = "Italiano"; English = "Italian"; Rtl = false }
          { Code = "ru"; Native = "Русский"; English = "Russian"; Rtl = false }
          { Code = "pl"; Native = "Polski"; English = "Polish"; Rtl = false }
          { Code = "nl"; Native = "Nederlands"; English = "Dutch"; Rtl = false }
          { Code = "cs"; Native = "Čeština"; English = "Czech"; Rtl = false }
          { Code = "tr"; Native = "Türkçe"; English = "Turkish"; Rtl = false }
          { Code = "vi"; Native = "Tiếng Việt"; English = "Vietnamese"; Rtl = false }
          { Code = "th"; Native = "ไทย"; English = "Thai"; Rtl = false }
          { Code = "id"; Native = "Bahasa Indonesia"; English = "Indonesian"; Rtl = false }
          { Code = "hi"; Native = "हिन्दी"; English = "Hindi"; Rtl = false }
          { Code = "ar"; Native = "العربية"; English = "Arabic"; Rtl = true } ]

    let private fallbackCode = "en"

    /// lang.<code>.json 리소스를 모두 읽어 언어별 사전으로 만든다.
    let private catalog : Dictionary<string, Dictionary<string, string>> =
        let result = Dictionary<string, Dictionary<string, string>>(StringComparer.OrdinalIgnoreCase)
        try
            let asm = Assembly.GetExecutingAssembly()
            for name in asm.GetManifestResourceNames() do
                if name.Contains ".lang." && name.EndsWith(".json", StringComparison.OrdinalIgnoreCase) then
                    let code = name.Substring(name.IndexOf ".lang." + 6).Replace(".json", "")
                    try
                        use stream = asm.GetManifestResourceStream name
                        use doc = JsonDocument.Parse(stream: Stream)
                        let table = Dictionary<string, string>(StringComparer.Ordinal)
                        for entry in doc.RootElement.EnumerateObject() do
                            if entry.Value.ValueKind = JsonValueKind.String then
                                table.[entry.Name] <- entry.Value.GetString()
                        result.[code] <- table
                    with _ -> ()
        with _ -> ()
        result

    /// 카탈로그에 실제로 들어 있는 언어만 노출한다.
    let availableLanguages () =
        languages |> List.filter (fun l -> catalog.ContainsKey l.Code)

    let private changedEvent = Event<string>()

    let mutable private currentCode = "ko"

    /// 언어가 바뀔 때 알림 (UI 전체 다시 그리기용)
    let changed = changedEvent.Publish

    let current () = currentCode

    let currentLanguage () =
        languages
        |> List.tryFind (fun l -> String.Equals(l.Code, currentCode, StringComparison.OrdinalIgnoreCase))
        |> Option.defaultValue languages.Head

    let isRtl () = (currentLanguage ()).Rtl

    /// OS 표시 언어에서 시작 언어를 고른다. 지원하지 않으면 영어.
    let detectSystemLanguage () =
        let ui = Globalization.CultureInfo.CurrentUICulture
        let exact =
            languages
            |> List.tryFind (fun l -> String.Equals(l.Code, ui.Name, StringComparison.OrdinalIgnoreCase))
        match exact with
        | Some l -> l.Code
        | None ->
            let twoLetter = ui.TwoLetterISOLanguageName
            if String.Equals(twoLetter, "zh", StringComparison.OrdinalIgnoreCase) then
                if ui.Name.Contains "Hant" || ui.Name.Contains "TW" || ui.Name.Contains "HK" || ui.Name.Contains "MO"
                then "zh-Hant"
                else "zh-Hans"
            else
                languages
                |> List.tryFind (fun l -> String.Equals(l.Code, twoLetter, StringComparison.OrdinalIgnoreCase))
                |> Option.map (fun l -> l.Code)
                |> Option.defaultValue fallbackCode

    let setLanguage (code: string) =
        let code =
            languages
            |> List.tryFind (fun l -> String.Equals(l.Code, code, StringComparison.OrdinalIgnoreCase))
            |> Option.map (fun l -> l.Code)
            |> Option.defaultValue fallbackCode
        if code <> currentCode then
            currentCode <- code
            changedEvent.Trigger code

    /// 키를 현재 언어 문자열로 바꾼다. 번역이 없으면 영어 -> 키 순서로 되돌아간다.
    let t (key: string) =
        let lookup (lang: string) =
            match catalog.TryGetValue lang with
            | true, table ->
                match table.TryGetValue key with
                | true, v when not (String.IsNullOrEmpty v) -> Some v
                | _ -> None
            | _ -> None
        match lookup currentCode with
        | Some v -> v
        | None ->
            match lookup fallbackCode with
            | Some v -> v
            | None -> key

    /// {0}, {1} 자리표시자를 채운다.
    let tf (key: string) ([<ParamArray>] args: obj[]) =
        let template = t key
        try String.Format(Globalization.CultureInfo.CurrentCulture, template, args)
        with _ -> template

    /// 화면 요소 종류 이름
    let kindLabel (kind: ItemKind) =
        match kind with
        | Switch -> t "type.switch"
        | Lamp -> t "type.lamp"
        | SwitchLamp -> t "type.switchLamp"
        | NumInput -> t "type.numInput"
        | NumDisplay -> t "type.numDisplay"
        | Text -> t "type.text"

    /// 스위치 동작 이름
    let actionLabel (action: SwitchAction) =
        match action with
        | Toggle -> t "action.toggle"
        | On -> t "action.on"
        | Off -> t "action.off"
        | Momentary -> t "action.momentary"
