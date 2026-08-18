namespace XgbHmi.App.Themes

open Avalonia.Media

/// 사용할 수 있는 테마 종류.
type ThemeId =
    | Light
    | Dark
    | Cyberpunk
    | Xg5000
    | Blueprint
    | Contrast

    member this.Code =
        match this with
        | Light -> "light"
        | Dark -> "dark"
        | Cyberpunk -> "cyberpunk"
        | Xg5000 -> "xg5000"
        | Blueprint -> "blueprint"
        | Contrast -> "contrast"

    member this.NameKey =
        match this with
        | Light -> "theme.light"
        | Dark -> "theme.dark"
        | Cyberpunk -> "theme.cyberpunk"
        | Xg5000 -> "theme.xg5000"
        | Blueprint -> "theme.blueprint"
        | Contrast -> "theme.contrast"

/// 테마 하나의 색 토큰 모음. 모든 화면은 이 토큰만 참조한다.
type Palette =
    { Id: ThemeId
      IsDark: bool
      /// 앱 바깥 배경 (도킹 영역)
      Window: string
      /// 패널/문서 배경
      Surface: string
      /// 입력 컨트롤, 교차 행 배경
      SurfaceAlt: string
      /// 메뉴바 / 툴바 / 패널 제목 표시줄
      Header: string
      /// 상태 표시줄
      StatusBar: string
      Border: string
      BorderStrong: string
      Text: string
      TextMuted: string
      TextInverse: string
      Accent: string
      AccentHover: string
      AccentText: string
      Selection: string
      CanvasBg: string
      CanvasGrid: string
      CardBg: string
      CardHeader: string
      CardBorder: string
      On: string
      Off: string
      Ok: string
      Warn: string
      Error: string
      KindSwitch: string
      KindLamp: string
      KindNumeric: string
      KindText: string
      /// 사이버펑크처럼 발광 효과를 쓸 때의 색 (없으면 투명)
      Glow: string }

[<RequireQualifiedAccess>]
module Palette =

    let light =
        { Id = Light
          IsDark = false
          Window = "#EEF1F5"
          Surface = "#FFFFFF"
          SurfaceAlt = "#F5F7FA"
          Header = "#FAFBFD"
          StatusBar = "#E4E9F0"
          Border = "#D5DAE1"
          BorderStrong = "#B4BCC7"
          Text = "#1B1F26"
          TextMuted = "#6A7480"
          TextInverse = "#FFFFFF"
          Accent = "#0F62FE"
          AccentHover = "#0043CE"
          AccentText = "#FFFFFF"
          Selection = "#CFE0FF"
          CanvasBg = "#F7F9FC"
          CanvasGrid = "#DFE5EE"
          CardBg = "#FFFFFF"
          CardHeader = "#EEF2F8"
          CardBorder = "#CBD3DE"
          On = "#12A150"
          Off = "#98A2B3"
          Ok = "#12A150"
          Warn = "#B25E00"
          Error = "#C21B17"
          KindSwitch = "#0F62FE"
          KindLamp = "#8A3FFC"
          KindNumeric = "#0072A3"
          KindText = "#6A7480"
          Glow = "#00000000" }

    let dark =
        { light with
            Id = Dark
            IsDark = true
            Window = "#15181D"
            Surface = "#1D2127"
            SurfaceAlt = "#252A32"
            Header = "#22272F"
            StatusBar = "#1A1E24"
            Border = "#2F353E"
            BorderStrong = "#454D59"
            Text = "#E4E9F0"
            TextMuted = "#98A2B3"
            TextInverse = "#0B0D10"
            Accent = "#4C8DFF"
            AccentHover = "#79A9FF"
            AccentText = "#0B0D10"
            Selection = "#2B3D5C"
            CanvasBg = "#161A20"
            CanvasGrid = "#242A32"
            CardBg = "#21262E"
            CardHeader = "#2A313A"
            CardBorder = "#39414C"
            On = "#2BD37E"
            Off = "#5A6472"
            Ok = "#2BD37E"
            Warn = "#F1A33B"
            Error = "#FF6B6B"
            KindSwitch = "#4C8DFF"
            KindLamp = "#B18AFF"
            KindNumeric = "#39C7D6"
            KindText = "#8C97A6"
            Glow = "#00000000" }

    let cyberpunk =
        { light with
            Id = Cyberpunk
            IsDark = true
            Window = "#08040F"
            Surface = "#12081F"
            SurfaceAlt = "#1B0F2E"
            Header = "#1A0B2E"
            StatusBar = "#0E0618"
            Border = "#3B1F63"
            BorderStrong = "#6B2FA8"
            Text = "#F0DBFF"
            TextMuted = "#A583C9"
            TextInverse = "#0A0312"
            Accent = "#FF2D95"
            AccentHover = "#FF6FB8"
            AccentText = "#12000A"
            Selection = "#3D1250"
            CanvasBg = "#0B0518"
            CanvasGrid = "#241041"
            CardBg = "#160A26"
            CardHeader = "#25103F"
            CardBorder = "#5A2790"
            On = "#00F5D4"
            Off = "#5B3D7A"
            Ok = "#00F5D4"
            Warn = "#FFD400"
            Error = "#FF3864"
            KindSwitch = "#FF2D95"
            KindLamp = "#FFD400"
            KindNumeric = "#00F5D4"
            KindText = "#9B6BFF"
            Glow = "#66FF2D95" }

    /// XG5000 / GX Works 같은 전통적인 엔지니어링 툴 색감
    let xg5000 =
        { light with
            Id = Xg5000
            IsDark = false
            Window = "#DFDCD3"
            Surface = "#FFFFFF"
            SurfaceAlt = "#F1EFE8"
            Header = "#E7E4DB"
            StatusBar = "#D4D0C6"
            Border = "#A9A499"
            BorderStrong = "#7C776C"
            Text = "#14171A"
            TextMuted = "#5A5750"
            TextInverse = "#FFFFFF"
            Accent = "#0A4C9B"
            AccentHover = "#0B62C6"
            AccentText = "#FFFFFF"
            Selection = "#B6D0F0"
            CanvasBg = "#FBFAF6"
            CanvasGrid = "#D9D5C9"
            CardBg = "#F6F5F0"
            CardHeader = "#DCE6F4"
            CardBorder = "#98938A"
            On = "#0E8A3E"
            Off = "#8E8A80"
            Ok = "#0E8A3E"
            Warn = "#9A6100"
            Error = "#B3261E"
            KindSwitch = "#0A4C9B"
            KindLamp = "#9A6100"
            KindNumeric = "#00707A"
            KindText = "#5A5750"
            Glow = "#00000000" }

    let blueprint =
        { light with
            Id = Blueprint
            IsDark = true
            Window = "#08192E"
            Surface = "#0E2440"
            SurfaceAlt = "#143050"
            Header = "#112C4C"
            StatusBar = "#0A1D34"
            Border = "#1E4472"
            BorderStrong = "#2E63A0"
            Text = "#DCEBFF"
            TextMuted = "#8FB2D9"
            TextInverse = "#04101F"
            Accent = "#52D1FF"
            AccentHover = "#8FE3FF"
            AccentText = "#04101F"
            Selection = "#1B4B7A"
            CanvasBg = "#0A1E36"
            CanvasGrid = "#173A61"
            CardBg = "#102A49"
            CardHeader = "#16375D"
            CardBorder = "#27568C"
            On = "#5BE7A9"
            Off = "#5E7FA3"
            Ok = "#5BE7A9"
            Warn = "#FFC24B"
            Error = "#FF7B7B"
            KindSwitch = "#52D1FF"
            KindLamp = "#FFC24B"
            KindNumeric = "#5BE7A9"
            KindText = "#8FB2D9"
            Glow = "#3352D1FF" }

    let contrast =
        { light with
            Id = Contrast
            IsDark = true
            Window = "#000000"
            Surface = "#000000"
            SurfaceAlt = "#101010"
            Header = "#000000"
            StatusBar = "#000000"
            Border = "#FFFFFF"
            BorderStrong = "#FFFFFF"
            Text = "#FFFFFF"
            TextMuted = "#D0D0D0"
            TextInverse = "#000000"
            Accent = "#FFFF00"
            AccentHover = "#FFFF7A"
            AccentText = "#000000"
            Selection = "#00427A"
            CanvasBg = "#000000"
            CanvasGrid = "#2A2A2A"
            CardBg = "#000000"
            CardHeader = "#141414"
            CardBorder = "#FFFFFF"
            On = "#00FF66"
            Off = "#909090"
            Ok = "#00FF66"
            Warn = "#FFB000"
            Error = "#FF5252"
            KindSwitch = "#FFFF00"
            KindLamp = "#FF9E00"
            KindNumeric = "#00E5FF"
            KindText = "#FFFFFF"
            Glow = "#00000000" }

    let all = [ light; dark; cyberpunk; xg5000; blueprint; contrast ]

    let byId (id: ThemeId) =
        all |> List.tryFind (fun p -> p.Id = id) |> Option.defaultValue light

    let byCode (code: string) =
        all
        |> List.tryFind (fun p -> System.String.Equals(p.Id.Code, code, System.StringComparison.OrdinalIgnoreCase))
        |> Option.defaultValue light

    let color (hex: string) = Color.Parse hex

    let brush (hex: string) : IBrush = SolidColorBrush(Color.Parse hex) :> IBrush
