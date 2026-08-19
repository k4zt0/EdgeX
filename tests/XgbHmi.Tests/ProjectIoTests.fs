module XgbHmi.Tests.ProjectIoTests

open System
open System.IO
open Xunit
open XgbHmi.Core

let private tempFile () =
    Path.Combine(Path.GetTempPath(), "xgbhmi_test_" + Guid.NewGuid().ToString("N") + ".xml")

[<Fact>]
let ``기본 예제는 원본 v6 배치를 그대로 만든다`` () =
    let project = Project.createDefault ()
    Assert.Equal(22, project.Items.Length)
    Assert.Equal("192.168.1.120", project.PlcIp)
    Assert.Equal(2004, project.Port)
    Assert.Equal(300, project.CycleMs)

    let first = project.Items.Head
    Assert.Equal(Switch, first.Kind)
    Assert.Equal("M01008", first.Device)
    Assert.Equal("P00120", first.MonitorDevice)
    Assert.Equal(Toggle, first.Action)
    Assert.Equal(18, first.X)
    Assert.Equal(18, first.Y)
    Assert.Equal(205, first.Width)
    Assert.Equal(105, first.Height)

[<Fact>]
let ``프로젝트를 저장하고 다시 읽으면 값이 같다`` () =
    let path = tempFile ()
    try
        let original = Project.createDefault ()
        ProjectIo.save path original
        let reloaded = ProjectIo.load path

        Assert.Equal(original.PlcIp, reloaded.PlcIp)
        Assert.Equal(original.Port, reloaded.Port)
        Assert.Equal(original.CycleMs, reloaded.CycleMs)
        Assert.Equal(original.Items.Length, reloaded.Items.Length)

        List.zip original.Items reloaded.Items
        |> List.iter (fun (a, b) ->
            Assert.Equal(a.Id, b.Id)
            Assert.Equal(a.Kind, b.Kind)
            Assert.Equal(a.Name, b.Name)
            Assert.Equal(a.Device, b.Device)
            Assert.Equal(a.MonitorDevice, b.MonitorDevice)
            Assert.Equal(a.Action, b.Action)
            Assert.Equal(a.Min, b.Min)
            Assert.Equal(a.Max, b.Max)
            Assert.Equal(a.X, b.X)
            Assert.Equal(a.Y, b.Y)
            Assert.Equal(a.Width, b.Width)
            Assert.Equal(a.Height, b.Height))
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``v6 윈도우판이 만든 XML 을 그대로 읽는다`` () =
    // XmlSerializer 가 만들던 형식 그대로 (네임스페이스 속성 포함)
    let xml =
        """<?xml version="1.0" encoding="utf-8"?>
<HmiProject xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema">
  <PlcIp>192.168.1.120</PlcIp>
  <Port>2004</Port>
  <CycleMs>300</CycleMs>
  <Items>
    <HmiItem>
      <Id>bf54b91b624a4157ae8227f9aa510fd7</Id>
      <Enabled>true</Enabled>
      <Type>SWITCH</Type>
      <Name>sys tr in enable c</Name>
      <Device>M01008</Device>
      <MonitorDevice>P00120</MonitorDevice>
      <Action>토글</Action>
      <Min>0</Min>
      <Max>65535</Max>
      <X>18</X>
      <Y>18</Y>
      <Width>205</Width>
      <Height>105</Height>
    </HmiItem>
    <HmiItem>
      <Id>a35c582c9b7746838eebd5406086b23f</Id>
      <Enabled>false</Enabled>
      <Type>NUM_INPUT</Type>
      <Name>D200 설정값</Name>
      <Device>D200</Device>
      <MonitorDevice />
      <Action>ON/OFF</Action>
      <Min>-32768</Min>
      <Max>65535</Max>
      <X>18</X>
      <Y>500</Y>
      <Width>250</Width>
      <Height>125</Height>
    </HmiItem>
  </Items>
</HmiProject>"""
    let path = tempFile ()
    try
        File.WriteAllText(path, xml)
        let project = ProjectIo.load path
        Assert.Equal(2, project.Items.Length)

        let sw = project.Items.[0]
        Assert.Equal(Switch, sw.Kind)
        Assert.True sw.Enabled
        Assert.Equal("bf54b91b624a4157ae8227f9aa510fd7", sw.Id)
        Assert.Equal(Toggle, sw.Action)

        let num = project.Items.[1]
        Assert.Equal(NumInput, num.Kind)
        Assert.False num.Enabled
        // 없앤 'ON/OFF' 동작은 예전 파일 호환을 위해 토글로 읽는다.
        Assert.Equal(Toggle, num.Action)
        Assert.Equal(-32768, num.Min)
        Assert.Equal("", num.MonitorDevice)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``요소 검사 규칙은 v6 와 같다`` () =
    let switch = { Item.create Switch with Device = "D200" }
    Assert.True(match Item.validate switch with Error _ -> true | Ok _ -> false)

    let numeric = { Item.create NumInput with Device = "M1000" }
    Assert.True(match Item.validate numeric with Error _ -> true | Ok _ -> false)

    let text = { Item.create Text with Device = "" }
    Assert.True(match Item.validate text with Ok _ -> true | Error _ -> false)

    let ok = { Item.create Switch with Device = "M01008"; MonitorDevice = "P00120" }
    Assert.True(match Item.validate ok with Ok _ -> true | Error _ -> false)

[<Fact>]
let ``폴링 주소 목록은 중복 없이 비트와 WORD 를 나눈다`` () =
    let items =
        [ { Item.create Switch with Device = "M01008"; MonitorDevice = "P00120" }
          { Item.create Switch with Device = "M01008"; MonitorDevice = "" }
          { Item.create Lamp with Device = "P00121" }
          { Item.create NumInput with Device = "D200" }
          { Item.create NumDisplay with Device = "D200" }
          { Item.create Text with Device = "" }
          { Item.create Switch with Device = "M01009"; Enabled = false } ]

    let bits, words = Project.scanAddresses items
    Assert.Equal<string list>([ "M01008"; "P00120"; "P00121" ], bits)
    Assert.Equal<string list>([ "D200" ], words)

[<Fact>]
let ``다음 M 주소는 가장 큰 주소 다음 번호다`` () =
    let items =
        [ { Item.create Switch with Device = "M01016" }
          { Item.create Switch with Device = "M00501" }
          { Item.create NumInput with Device = "D200" } ]
    Assert.Equal(1017, Project.nextMAddress items)
    Assert.Equal(1000, Project.nextMAddress [])

[<Fact>]
let ``도면 크기를 저장하고 다시 읽는다`` () =
    let path = tempFile ()
    try
        let project = { Project.createDefault () with ScreenWidth = 2400; ScreenHeight = 1500 }
        ProjectIo.save path project
        let reloaded = ProjectIo.load path
        Assert.Equal(2400, reloaded.ScreenWidth)
        Assert.Equal(1500, reloaded.ScreenHeight)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``도면 크기가 없는 v6 파일은 기본값으로 열린다`` () =
    let path = tempFile ()
    try
        let xml =
            """<?xml version="1.0" encoding="utf-8"?>
<HmiProject>
  <PlcIp>192.168.1.120</PlcIp>
  <Port>2004</Port>
  <CycleMs>300</CycleMs>
  <Items />
</HmiProject>"""
        File.WriteAllText(path, xml)
        let project = ProjectIo.load path
        Assert.Equal(Limits.defaultScreenWidth, project.ScreenWidth)
        Assert.Equal(Limits.defaultScreenHeight, project.ScreenHeight)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``요소가 도면 밖에 있으면 도면을 넓혀서 연다`` () =
    let far =
        { Item.create Switch with
            Device = "M01000"
            X = 3000
            Y = 2200
            Width = 205
            Height = 105 }
    let project = { Project.empty with Items = [ far ]; ScreenWidth = 800; ScreenHeight = 600 }
    let fitted = Project.fitScreen project
    Assert.True(fitted.ScreenWidth >= 3000 + 205)
    Assert.True(fitted.ScreenHeight >= 2200 + 105)

[<Fact>]
let ``스위치_램프는 저장하고 다시 읽어도 종류가 유지된다`` () =
    let path = tempFile ()
    try
        let item =
            { Item.create SwitchLamp with
                Name = "펌프 기동"
                Device = "M1000"
                MonitorDevice = "P00120" }
        Assert.Equal("SWITCH_LAMP", item.Kind.Code)

        ProjectIo.save path { Project.empty with Items = [ item ] }
        let reloaded = ProjectIo.load path

        let back = reloaded.Items.Head
        Assert.Equal(SwitchLamp, back.Kind)
        Assert.Equal("펌프 기동", back.Name)
        Assert.Equal("M1000", back.Device)
        Assert.Equal("P00120", back.MonitorDevice)
        Assert.Equal(Toggle, back.Action)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``스위치_램프는 비트 주소를 쓰고 조작 버튼을 가진다`` () =
    Assert.True(ItemKind.isBit SwitchLamp)
    Assert.False(ItemKind.isWord SwitchLamp)
    Assert.True(ItemKind.hasAction SwitchLamp)
    Assert.True(ItemKind.hasAction Switch)
    Assert.False(ItemKind.hasAction Lamp)

    // 비트 종류이므로 M/P 가 아니면 검사에서 걸린다.
    match Item.validate { Item.create SwitchLamp with Device = "D100" } with
    | Error _ -> ()
    | Ok() -> failwith "D 주소는 걸러져야 한다"
    match Item.validate { Item.create SwitchLamp with Device = "M1000" } with
    | Ok() -> ()
    | Error m -> failwith m

[<Fact>]
let ``없앤 ON_OFF 동작은 토글로 읽고 목록에는 없다`` () =
    Assert.Equal(Toggle, SwitchAction.parse "ON/OFF")
    Assert.Equal<SwitchAction list>([ Toggle; On; Off; Momentary ], SwitchAction.all)
    Assert.DoesNotContain("ON/OFF", SwitchAction.codes)

[<Fact>]
let ``통합 스위치는 제 주소 없이도 통과하고 저장된다`` () =
    let path = tempFile ()
    try
        let item = { Item.create MasterSwitch with Name = "통합 조작" }
        Assert.Equal("MASTER_SWITCH", item.Kind.Code)
        Assert.Equal("", item.Device)
        // 대상 요소의 주소를 빌려 쓰므로 제 주소는 검사하지 않는다.
        match Item.validate item with
        | Ok() -> ()
        | Error m -> failwith m
        Assert.False(ItemKind.isBit MasterSwitch)
        Assert.False(ItemKind.isWord MasterSwitch)
        Assert.False(ItemKind.hasAction MasterSwitch)

        ProjectIo.save path { Project.empty with Items = [ item ] }
        let back = (ProjectIo.load path).Items.Head
        Assert.Equal(MasterSwitch, back.Kind)
        Assert.Equal("통합 조작", back.Name)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``통합 스위치는 폴링 주소를 만들지 않는다`` () =
    let items =
        [ { Item.create Switch with Device = "M1000"; MonitorDevice = "P00120" }
          { Item.create MasterSwitch with Name = "통합 조작" }
          { Item.create NumDisplay with Device = "D100" } ]
    let bits, words = Project.scanAddresses items
    Assert.Equal<string list>([ "M1000"; "P00120" ], bits)
    Assert.Equal<string list>([ "D100" ], words)

[<Fact>]
let ``운전 화면 표시는 저장되고 예전 파일은 모두 보인다`` () =
    let path = tempFile ()
    try
        let items =
            [ { Item.create Switch with Name = "숨김"; Device = "M1000"; Visible = false }
              { Item.create Switch with Name = "보임"; Device = "M1001" } ]
        ProjectIo.save path { Project.empty with Items = items }
        let back = (ProjectIo.load path).Items
        Assert.False back.[0].Visible
        Assert.True back.[1].Visible
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``Visible 항목이 없는 v6 파일은 전부 보이게 읽는다`` () =
    let xml =
        """<?xml version="1.0" encoding="utf-8"?>
<HmiProject>
  <PlcIp>192.168.1.120</PlcIp>
  <Port>2004</Port>
  <CycleMs>300</CycleMs>
  <Items>
    <HmiItem>
      <Id>a</Id>
      <Enabled>true</Enabled>
      <Type>SWITCH</Type>
      <Name>스위치</Name>
      <Device>M1000</Device>
      <MonitorDevice />
      <Action>토글</Action>
    </HmiItem>
  </Items>
</HmiProject>"""
    let path = tempFile ()
    try
        File.WriteAllText(path, xml)
        Assert.True (ProjectIo.load path).Items.Head.Visible
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``통합 스위치 기본 크기는 내용이 들어갈 만큼 넉넉하다`` () =
    let item = Item.create MasterSwitch
    // 대상 고르기 + 목록 + 조작 버튼 + 전체 종료가 한 장에 들어가야 한다.
    Assert.Equal(320, item.Width)
    Assert.Equal(380, item.Height)

// ---------------------------------------------------------------------------
//  터치스크린(HMI) 화면
// ---------------------------------------------------------------------------

[<Fact>]
let ``HMI 부품을 저장하고 다시 읽으면 값이 같다`` () =
    let path = tempFile ()
    try
        let switch = { Item.create Switch with Id = "sw1"; Device = "M1000" }
        let button =
            { HmiPart.create PartButton with
                Id = "pt1"
                TargetId = "sw1"
                Text = "기동"
                OnText = "RUN"
                OffText = "STOP"
                X = 120
                Y = 64
                Width = 160
                Height = 160
                Shape = HmiShape.circle
                OnColor = "#2FA84F"
                OffColor = "#1E232B"
                FontSize = 22 }
        let gauge =
            { HmiPart.create PartGauge with
                Id = "pt2"
                TargetId = "num1"
                Unit = "℃"
                X = 400
                Y = 64 }

        let original =
            { Project.empty with
                Items = [ switch ]
                Hmi =
                    { Width = 1280
                      Height = 800
                      Background = "#101318"
                      Parts = [ button; gauge ] } }

        ProjectIo.save path original
        let reloaded = ProjectIo.load path

        Assert.Equal(1280, reloaded.Hmi.Width)
        Assert.Equal(800, reloaded.Hmi.Height)
        Assert.Equal("#101318", reloaded.Hmi.Background)
        Assert.Equal(2, reloaded.Hmi.Parts.Length)

        let b = reloaded.Hmi.Parts.Head
        Assert.Equal(PartButton, b.Kind)
        Assert.Equal("sw1", b.TargetId)
        Assert.Equal("기동", b.Text)
        Assert.Equal("RUN", b.OnText)
        Assert.Equal("STOP", b.OffText)
        Assert.Equal(HmiShape.circle, b.Shape)
        Assert.Equal("#2FA84F", b.OnColor)
        Assert.Equal(22, b.FontSize)
        Assert.Equal(120, b.X)
        Assert.Equal(160, b.Width)

        let g = reloaded.Hmi.Parts.[1]
        Assert.Equal(PartGauge, g.Kind)
        Assert.Equal("num1", g.TargetId)
        Assert.Equal("℃", g.Unit)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``HMI 항목이 없는 예전 파일도 그대로 열린다`` () =
    let path = tempFile ()
    try
        // v6 가 저장한 파일에는 <Hmi> 가 없다. 빈 터치스크린으로 읽혀야 한다.
        File.WriteAllText(
            path,
            """<?xml version="1.0" encoding="utf-8"?>
<HmiProject>
  <PlcIp>192.168.1.120</PlcIp>
  <Port>2004</Port>
  <CycleMs>300</CycleMs>
  <Items>
    <HmiItem>
      <Type>SWITCH</Type>
      <Name>job start</Name>
      <Device>M01009</Device>
      <Action>토글</Action>
    </HmiItem>
  </Items>
</HmiProject>"""
        )

        let loaded = ProjectIo.load path
        Assert.Single loaded.Items |> ignore
        Assert.Empty loaded.Hmi.Parts
        Assert.Equal(HmiLimits.defaultWidth, loaded.Hmi.Width)
        Assert.Equal(HmiLimits.defaultHeight, loaded.Hmi.Height)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``HMI 부품 값은 저장할 때 안전한 범위로 맞춰진다`` () =
    let path = tempFile ()
    try
        let broken =
            { HmiPart.create PartValue with
                X = -50
                Y = -10
                Width = 2
                Height = 1
                FontSize = 500
                Corner = 999
                Align = "위쪽"
                Shape = "삼각형"
                OnColor = "빨강" }

        let project = { Project.empty with Hmi = { HmiScreen.empty with Parts = [ broken ] } }
        ProjectIo.save path project
        let part = (ProjectIo.load path).Hmi.Parts.Head

        Assert.Equal(0, part.X)
        Assert.Equal(0, part.Y)
        Assert.Equal(HmiLimits.minPartWidth, part.Width)
        Assert.Equal(HmiLimits.minPartHeight, part.Height)
        Assert.Equal(HmiLimits.maxFontSize, part.FontSize)
        Assert.Equal(60, part.Corner)
        Assert.Equal("CENTER", part.Align)
        Assert.Equal(HmiShape.rect, part.Shape)
        // 색은 #RRGGBB 만 받는다. 아니면 테마 기본으로 되돌린다.
        Assert.Equal("", part.OnColor)
    finally
        if File.Exists path then File.Delete path

[<Fact>]
let ``부품 동작 코드는 스위치 동작 코드와 같아야 한다`` () =
    // 부품에서 고른 동작은 SwitchAction.parse 로 해석된다.
    // 두 목록이 어긋나면 버튼이 조용히 '토글' 로 떨어지므로 여기서 묶어 둔다.
    Assert.Equal<string list>("" :: SwitchAction.codes, HmiPart.actionCodes)
    for code in SwitchAction.codes do
        Assert.Equal(code, HmiPart.normalizeAction code)
    Assert.Equal("", HmiPart.normalizeAction "없는동작")

[<Fact>]
let ``새 부품 종류와 설정도 저장하고 다시 읽으면 같다`` () =
    let path = tempFile ()
    try
        let rotary =
            { HmiPart.create PartRotary with
                Id = "r1"
                TargetId = "mode"
                Count = 3
                Options = "LOW|MID|HIGH" }
        let arrow = { HmiPart.create PartArrow with Id = "a1"; TargetId = "sv"; Step = -5 }
        let preset = { HmiPart.create PartSetValue with Id = "s1"; TargetId = "sv"; WriteValue = 480 }
        let array' = { HmiPart.create PartLampArray with Id = "l1"; TargetId = "in"; Count = 8; Vertical = true }
        let bar = { HmiPart.create PartBar with Id = "b1"; TargetId = "pv"; Decimals = 1; Unit = "%" }
        let clock = { HmiPart.create PartClock with Id = "c1"; Text = "HH:mm" }
        let onButton = { HmiPart.create PartButton with Id = "on1"; TargetId = "coil"; Action = "ON" }
        let offButton = { HmiPart.create PartButton with Id = "off1"; TargetId = "coil"; Action = "OFF" }

        let project =
            { Project.empty with
                Hmi =
                    { HmiScreen.empty with
                        Parts = [ rotary; arrow; preset; array'; bar; clock; onButton; offButton ] } }

        ProjectIo.save path project
        let parts = (ProjectIo.load path).Hmi.Parts
        Assert.Equal(8, parts.Length)

        let byId id = parts |> List.find (fun p -> p.Id = id)
        Assert.Equal(PartRotary, (byId "r1").Kind)
        Assert.Equal(3, (byId "r1").Count)
        Assert.Equal("LOW|MID|HIGH", (byId "r1").Options)
        // 삼각 버튼은 증감폭의 부호가 방향이므로 음수가 살아 있어야 한다.
        Assert.Equal(-5, (byId "a1").Step)
        Assert.Equal(480, (byId "s1").WriteValue)
        Assert.True((byId "l1").Vertical)
        Assert.Equal(8, (byId "l1").Count)
        Assert.Equal(1, (byId "b1").Decimals)
        Assert.Equal("%", (byId "b1").Unit)
        Assert.Equal(PartClock, (byId "c1").Kind)
        Assert.Equal("HH:mm", (byId "c1").Text)
        // 같은 코일에 ON / OFF 버튼을 따로 둘 수 있어야 한다.
        Assert.Equal("ON", (byId "on1").Action)
        Assert.Equal("OFF", (byId "off1").Action)
        Assert.Equal((byId "on1").TargetId, (byId "off1").TargetId)
    finally
        if File.Exists path then File.Delete path
