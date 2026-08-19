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
