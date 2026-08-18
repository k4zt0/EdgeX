namespace XgbHmi.Core

/// 정렬 기준
type AlignMode =
    | Left
    | CenterHorizontal
    | Right
    | Top
    | Middle
    | Bottom

/// 화면 요소 자동 정렬. 순수 계산만 하므로 그대로 시험할 수 있다.
[<RequireQualifiedAccess>]
module Layout =

    /// 자동 배치에서 쓰는 여백/간격
    let margin = 18
    let gapX = 15
    let gapY = 13
    let defaultCanvasWidth = 1180

    /// 겹치지 않게 격자로 다시 깔아 준다.
    /// 현재 위치(위->아래, 왼쪽->오른쪽) 순서를 지키고, 행마다 가장 높은 요소에 맞춰 줄을 바꾼다.
    let autoArrange (canvasWidth: int) (items: HmiItem list) : HmiItem list =
        if items.IsEmpty then items
        else
            let width = max 400 canvasWidth

            // 원래 배치 순서를 유지하기 위해 y(80px 단위 행) -> x 순으로 정렬한다.
            let ordered =
                items
                |> List.sortBy (fun h -> (h.Y / 80, h.X, h.Name))

            let mutable cursorX = margin
            let mutable cursorY = margin
            let mutable rowHeight = 0
            let result = ResizeArray<HmiItem>()

            for item in ordered do
                if cursorX > margin && cursorX + item.Width > width - margin then
                    // 줄 바꿈
                    cursorX <- margin
                    cursorY <- cursorY + rowHeight + gapY
                    rowHeight <- 0
                result.Add { item with X = cursorX; Y = cursorY }
                cursorX <- cursorX + item.Width + gapX
                rowHeight <- max rowHeight item.Height

            // 입력 순서 그대로 돌려준다 (표/트리 순서를 흔들지 않기 위해)
            let byId = result |> Seq.map (fun h -> h.Id, h) |> dict
            items |> List.map (fun h -> match byId.TryGetValue h.Id with | true, v -> v | _ -> h)

    /// 선택한 요소들을 한 기준선에 맞춘다.
    let align (mode: AlignMode) (items: HmiItem list) : HmiItem list =
        if items.Length < 2 then items
        else
            let left = items |> List.map (fun h -> h.X) |> List.min
            let right = items |> List.map (fun h -> h.X + h.Width) |> List.max
            let top = items |> List.map (fun h -> h.Y) |> List.min
            let bottom = items |> List.map (fun h -> h.Y + h.Height) |> List.max
            let centerX = (left + right) / 2
            let centerY = (top + bottom) / 2

            items
            |> List.map (fun h ->
                match mode with
                | Left -> { h with X = left }
                | Right -> { h with X = max 0 (right - h.Width) }
                | CenterHorizontal -> { h with X = max 0 (centerX - h.Width / 2) }
                | Top -> { h with Y = top }
                | Bottom -> { h with Y = max 0 (bottom - h.Height) }
                | Middle -> { h with Y = max 0 (centerY - h.Height / 2) })

    /// 가로(또는 세로) 간격을 균등하게 만든다. 양 끝 요소는 움직이지 않는다.
    let distribute (horizontal: bool) (items: HmiItem list) : HmiItem list =
        if items.Length < 3 then items
        else
            let sorted =
                if horizontal then items |> List.sortBy (fun h -> h.X)
                else items |> List.sortBy (fun h -> h.Y)

            let first = List.head sorted
            let last = List.last sorted

            let spanStart = if horizontal then first.X else first.Y
            let spanEnd = if horizontal then last.X + last.Width else last.Y + last.Height
            let usedSize =
                sorted |> List.sumBy (fun h -> if horizontal then h.Width else h.Height)
            let gap = (spanEnd - spanStart - usedSize) / (sorted.Length - 1)

            let mutable cursor = spanStart
            let moved =
                sorted
                |> List.map (fun h ->
                    let placed = if horizontal then { h with X = max 0 cursor } else { h with Y = max 0 cursor }
                    cursor <- cursor + (if horizontal then h.Width else h.Height) + gap
                    placed)

            let byId = moved |> Seq.map (fun h -> h.Id, h) |> dict
            items |> List.map (fun h -> match byId.TryGetValue h.Id with | true, v -> v | _ -> h)

    /// 기준 요소의 크기에 나머지를 맞춘다.
    let matchSize (reference: HmiItem) (items: HmiItem list) : HmiItem list =
        items
        |> List.map (fun h ->
            if h.Id = reference.Id then h
            else { h with Width = reference.Width; Height = reference.Height })
