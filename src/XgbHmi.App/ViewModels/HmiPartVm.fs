namespace XgbHmi.App.ViewModels

open System
open System.ComponentModel
open XgbHmi.Core

/// 터치스크린 부품 하나를 캔버스와 속성창이 함께 편집할 수 있게 감싼 것.
/// 어느 쪽에서 값을 바꿔도 PropertyChanged 로 나머지 화면이 따라간다.
type HmiPartVm(part: HmiPart) as this =

    let propertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    let mutable p = HmiPart.normalize part
    let mutable beforeChange: unit -> unit = fun () -> ()

    let raise' (name: string) =
        propertyChanged.Trigger(this, PropertyChangedEventArgs name)

    /// 값이 실제로 바뀔 때만 되돌리기 스냅숏을 남기고 알림을 올린다.
    let set (name: string) (changed: bool) (apply: unit -> unit) =
        if changed then
            beforeChange ()
            apply ()
            raise' name

    member _.PropertyChangedEvent = propertyChanged.Publish

    member _.SetBeforeChangeHook(hook: unit -> unit) = beforeChange <- hook

    member _.Id = p.Id

    member _.Kind
        with get () = p.Kind
        and set (v: HmiPartKind) = set "Kind" (p.Kind <> v) (fun () -> p <- { p with Kind = v })

    member _.TargetId
        with get () = p.TargetId
        and set (v: string) =
            let v = if isNull v then "" else v.Trim()
            set "TargetId" (p.TargetId <> v) (fun () -> p <- { p with TargetId = v })

    member _.SubTargetId
        with get () = p.SubTargetId
        and set (v: string) =
            let v = if isNull v then "" else v.Trim()
            set "SubTargetId" (p.SubTargetId <> v) (fun () -> p <- { p with SubTargetId = v })

    member _.Text
        with get () = p.Text
        and set (v: string) =
            let v = if isNull v then "" else v
            set "Text" (p.Text <> v) (fun () -> p <- { p with Text = v })

    member _.OnText
        with get () = p.OnText
        and set (v: string) =
            let v = if isNull v then "" else v
            set "OnText" (p.OnText <> v) (fun () -> p <- { p with OnText = v })

    member _.OffText
        with get () = p.OffText
        and set (v: string) =
            let v = if isNull v then "" else v
            set "OffText" (p.OffText <> v) (fun () -> p <- { p with OffText = v })

    member _.Unit
        with get () = p.Unit
        and set (v: string) =
            let v = if isNull v then "" else v
            set "Unit" (p.Unit <> v) (fun () -> p <- { p with Unit = v })

    member _.X
        with get () = p.X
        and set (v: int) =
            let v = max 0 v
            set "X" (p.X <> v) (fun () -> p <- { p with X = v })

    member _.Y
        with get () = p.Y
        and set (v: int) =
            let v = max 0 v
            set "Y" (p.Y <> v) (fun () -> p <- { p with Y = v })

    member _.Width
        with get () = p.Width
        and set (v: int) =
            let v = max HmiLimits.minPartWidth v
            set "Width" (p.Width <> v) (fun () -> p <- { p with Width = v })

    member _.Height
        with get () = p.Height
        and set (v: int) =
            let v = max HmiLimits.minPartHeight v
            set "Height" (p.Height <> v) (fun () -> p <- { p with Height = v })

    member _.Shape
        with get () = p.Shape
        and set (v: string) =
            let v = HmiShape.normalize v
            set "Shape" (p.Shape <> v) (fun () -> p <- { p with Shape = v })

    member _.OffColor
        with get () = p.OffColor
        and set (v: string) =
            let v = HmiPart.normalizeColor v
            set "OffColor" (p.OffColor <> v) (fun () -> p <- { p with OffColor = v })

    member _.OnColor
        with get () = p.OnColor
        and set (v: string) =
            let v = HmiPart.normalizeColor v
            set "OnColor" (p.OnColor <> v) (fun () -> p <- { p with OnColor = v })

    member _.TextColor
        with get () = p.TextColor
        and set (v: string) =
            let v = HmiPart.normalizeColor v
            set "TextColor" (p.TextColor <> v) (fun () -> p <- { p with TextColor = v })

    member _.BorderColor
        with get () = p.BorderColor
        and set (v: string) =
            let v = HmiPart.normalizeColor v
            set "BorderColor" (p.BorderColor <> v) (fun () -> p <- { p with BorderColor = v })

    member _.FontSize
        with get () = p.FontSize
        and set (v: int) =
            let v = max HmiLimits.minFontSize (min HmiLimits.maxFontSize v)
            set "FontSize" (p.FontSize <> v) (fun () -> p <- { p with FontSize = v })

    member _.Corner
        with get () = p.Corner
        and set (v: int) =
            let v = max 0 (min 60 v)
            set "Corner" (p.Corner <> v) (fun () -> p <- { p with Corner = v })

    member _.Align
        with get () = p.Align
        and set (v: string) =
            let v = HmiPart.normalizeAlign v
            set "Align" (p.Align <> v) (fun () -> p <- { p with Align = v })

    member _.Step
        with get () = p.Step
        and set (v: int) =
            let v = max 0 (min 10000 v)
            set "Step" (p.Step <> v) (fun () -> p <- { p with Step = v })

    /// 연결한 요소의 스위치 동작을 덮어쓴다. 비우면 요소 설정 그대로.
    member _.Action
        with get () = p.Action
        and set (v: string) =
            let v = HmiPart.normalizeAction v
            set "Action" (p.Action <> v) (fun () -> p <- { p with Action = v })

    member _.Count
        with get () = p.Count
        and set (v: int) =
            let v = max 1 (min 16 v)
            set "Count" (p.Count <> v) (fun () -> p <- { p with Count = v })

    member _.Decimals
        with get () = p.Decimals
        and set (v: int) =
            let v = max 0 (min 3 v)
            set "Decimals" (p.Decimals <> v) (fun () -> p <- { p with Decimals = v })

    member _.WriteValue
        with get () = p.WriteValue
        and set (v: int) =
            let v = max -32768 (min 65535 v)
            set "WriteValue" (p.WriteValue <> v) (fun () -> p <- { p with WriteValue = v })

    member _.Options
        with get () = p.Options
        and set (v: string) =
            let v = if isNull v then "" else v
            set "Options" (p.Options <> v) (fun () -> p <- { p with Options = v })

    member _.Vertical
        with get () = p.Vertical
        and set (v: bool) = set "Vertical" (p.Vertical <> v) (fun () -> p <- { p with Vertical = v })

    /// 계기·막대의 눈금 최소. 최대보다 작을 때만 쓴다.
    member _.ScaleMin
        with get () = p.ScaleMin
        and set (v: int) =
            let v = max -32768 (min 65535 v)
            set "ScaleMin" (p.ScaleMin <> v) (fun () -> p <- { p with ScaleMin = v })

    member _.ScaleMax
        with get () = p.ScaleMax
        and set (v: int) =
            let v = max -32768 (min 65535 v)
            set "ScaleMax" (p.ScaleMax <> v) (fun () -> p <- { p with ScaleMax = v })

    /// 누른 뒤 ON 으로 만들 요소.
    member _.ThenOnId
        with get () = p.ThenOnId
        and set (v: string) =
            let v = (if isNull v then "" else v).Trim()
            set "ThenOnId" (p.ThenOnId <> v) (fun () -> p <- { p with ThenOnId = v })

    /// 상호 배타 버튼 그룹. 같은 이름끼리 한 번에 하나만 켜진다.
    member _.Group
        with get () = p.Group
        and set (v: string) =
            let v = (if isNull v then "" else v).Trim()
            set "Group" (p.Group <> v) (fun () -> p <- { p with Group = v })

    /// 캔버스 드래그/크기조절에서 한 번에 반영 (알림은 항목마다 한 번씩)
    member this.SetBounds(nx: int, ny: int, nw: int, nh: int) =
        this.X <- nx
        this.Y <- ny
        this.Width <- nw
        this.Height <- nh

    member _.ToPart() : HmiPart = p

    member _.KindLabel = I18n.partLabel p.Kind

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member _.PropertyChanged = propertyChanged.Publish
