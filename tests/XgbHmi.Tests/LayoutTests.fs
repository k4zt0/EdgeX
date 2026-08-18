module XgbHmi.Tests.LayoutTests

open Xunit
open XgbHmi.Core

let private item id x y w h =
    { Item.create Switch with Id = id; X = x; Y = y; Width = w; Height = h }

let private overlaps (a: HmiItem) (b: HmiItem) =
    a.X < b.X + b.Width && b.X < a.X + a.Width && a.Y < b.Y + b.Height && b.Y < a.Y + a.Height

[<Fact>]
let ``자동 배치는 겹침을 없애고 캔버스 폭 안에 넣는다`` () =
    // 일부러 전부 같은 자리에 겹쳐 둔다.
    let items = [ for i in 1..12 -> item (string i) 30 30 205 105 ]
    let arranged = Layout.autoArrange 900 items

    Assert.Equal(items.Length, arranged.Length)
    for a in arranged do
        Assert.True(a.X >= 0 && a.Y >= 0)
        Assert.True(a.X + a.Width <= 900, sprintf "요소가 캔버스를 넘어감: X=%d W=%d" a.X a.Width)

    for i in 0 .. arranged.Length - 1 do
        for j in i + 1 .. arranged.Length - 1 do
            Assert.False(overlaps arranged.[i] arranged.[j], sprintf "%s 와 %s 가 겹침" arranged.[i].Id arranged.[j].Id)

[<Fact>]
let ``자동 배치는 원래 순서(위->아래, 왼쪽->오른쪽)를 지킨다`` () =
    let items =
        [ item "c" 500 20 100 60
          item "a" 20 20 100 60
          item "b" 260 20 100 60
          item "d" 20 200 100 60 ]
    let arranged = Layout.autoArrange 800 items

    // 배치 결과를 읽는 순서(위->아래, 왼쪽->오른쪽)로 늘어놓으면 원래 순서와 같아야 한다.
    let readingOrder =
        arranged
        |> List.sortBy (fun h -> (h.Y, h.X))
        |> List.map (fun h -> h.Id)

    Assert.Equal<string list>([ "a"; "b"; "c"; "d" ], readingOrder)

[<Fact>]
let ``자동 배치는 폭이 모자라면 줄을 바꾼다`` () =
    let items = [ for i in 1..6 -> item (string i) (i * 10) 20 300 100 ]
    let arranged = Layout.autoArrange 700 items
    let rows = arranged |> List.map (fun h -> h.Y) |> List.distinct
    Assert.True(rows.Length >= 3, sprintf "줄 수가 %d 개뿐" rows.Length)

[<Fact>]
let ``왼쪽 오른쪽 가운데 맞춤`` () =
    let items = [ item "a" 10 10 100 50; item "b" 200 80 60 50; item "c" 90 150 140 50 ]

    let left = Layout.align Left items
    Assert.All(left, fun h -> Assert.Equal(10, h.X))

    let right = Layout.align Right items
    Assert.All(right, fun h -> Assert.Equal(260, h.X + h.Width))

    let center = Layout.align CenterHorizontal items
    let centers = center |> List.map (fun h -> h.X + h.Width / 2) |> List.distinct
    Assert.Single centers |> ignore

[<Fact>]
let ``위 아래 중간 맞춤`` () =
    let items = [ item "a" 10 10 100 50; item "b" 200 80 60 40; item "c" 90 150 140 60 ]

    let top = Layout.align Top items
    Assert.All(top, fun h -> Assert.Equal(10, h.Y))

    let bottom = Layout.align Bottom items
    Assert.All(bottom, fun h -> Assert.Equal(210, h.Y + h.Height))

    let middle = Layout.align Middle items
    let centers = middle |> List.map (fun h -> h.Y + h.Height / 2) |> List.distinct
    Assert.Single centers |> ignore

[<Fact>]
let ``가로 간격 균등은 양 끝을 두고 사이를 고르게 만든다`` () =
    let items = [ item "a" 0 0 100 50; item "b" 120 0 100 50; item "c" 600 0 100 50 ]
    let spread = Layout.distribute true items |> List.sortBy (fun h -> h.X)

    Assert.Equal(0, spread.Head.X)
    Assert.Equal(700, (List.last spread).X + (List.last spread).Width)

    let gaps =
        spread
        |> List.pairwise
        |> List.map (fun (a, b) -> b.X - (a.X + a.Width))
    Assert.All(gaps, fun g -> Assert.InRange(g, gaps.Head - 1, gaps.Head + 1))

[<Fact>]
let ``세로 간격 균등`` () =
    let items = [ item "a" 0 0 100 50; item "b" 0 60 100 50; item "c" 0 400 100 50 ]
    let spread = Layout.distribute false items |> List.sortBy (fun h -> h.Y)
    Assert.Equal(0, spread.Head.Y)
    Assert.Equal(450, (List.last spread).Y + (List.last spread).Height)

[<Fact>]
let ``크기 맞춤은 기준 요소만 그대로 둔다`` () =
    let reference = item "a" 0 0 205 105
    let items = [ reference; item "b" 300 0 120 60; item "c" 500 0 90 200 ]
    let sized = Layout.matchSize reference items

    Assert.All(sized, fun h ->
        Assert.Equal(205, h.Width)
        Assert.Equal(105, h.Height))

[<Fact>]
let ``선택이 부족하면 아무것도 바꾸지 않는다`` () =
    let one = [ item "a" 10 10 100 50 ]
    Assert.Equal<HmiItem list>(one, Layout.align Left one)
    Assert.Equal<HmiItem list>(one, Layout.distribute true one)

    let two = [ item "a" 10 10 100 50; item "b" 300 10 100 50 ]
    Assert.Equal<HmiItem list>(two, Layout.distribute true two)
