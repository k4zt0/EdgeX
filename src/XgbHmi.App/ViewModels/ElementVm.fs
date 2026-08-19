namespace XgbHmi.App.ViewModels

open System
open System.ComponentModel
open System.Runtime.CompilerServices
open XgbHmi.Core

/// 화면 요소 하나를 표/속성창/캔버스가 함께 편집할 수 있게 감싼 것.
/// 어느 쪽에서 값을 바꿔도 PropertyChanged 로 나머지 화면이 따라간다.
type ElementVm(item: HmiItem) as this =

    let propertyChanged = Event<PropertyChangedEventHandler, PropertyChangedEventArgs>()

    let mutable id = item.Id
    let mutable enabled = item.Enabled
    let mutable visible = item.Visible
    let mutable kind = item.Kind
    let mutable name = item.Name
    let mutable device = item.Device
    let mutable monitor = item.MonitorDevice
    let mutable action = item.Action
    let mutable minValue = item.Min
    let mutable maxValue = item.Max
    let mutable x = item.X
    let mutable y = item.Y
    let mutable width = item.Width
    let mutable height = item.Height

    /// 운전 중 오류 표시용. 프로젝트 파일에 저장하지 않고 PropertyChanged 도 올리지 않는다.
    /// (알림을 올리면 통신 오류만으로 프로젝트가 '수정됨'이 되어 버린다)
    let mutable fault: string option = None

    let mutable beforeChange: unit -> unit = fun () -> ()

    let raise' (propertyName: string) =
        propertyChanged.Trigger(this, PropertyChangedEventArgs propertyName)

    member _.PropertyChangedEvent = propertyChanged.Publish

    /// 값이 바뀌기 직전에 불린다. (되돌리기 스냅숏을 남기기 위한 것)
    member _.SetBeforeChangeHook(hook: unit -> unit) = beforeChange <- hook

    member _.Id = id

    member _.Enabled
        with get () = enabled
        and set (v: bool) =
            if enabled <> v then
                beforeChange ()
                enabled <- v
                raise' "Enabled"

    /// 운전 화면에 카드로 띄울지
    member _.Visible
        with get () = visible
        and set (v: bool) =
            if visible <> v then
                beforeChange ()
                visible <- v
                raise' "Visible"

    member _.Kind
        with get () = kind
        and set (v: ItemKind) =
            if kind <> v then
                beforeChange ()
                kind <- v
                raise' "Kind"
                raise' "KindIndex"
                raise' "KindLabel"

    /// 콤보 상자용 (0..4)
    member this.KindIndex
        with get () = ItemKind.all |> List.findIndex (fun k -> k = kind)
        and set (v: int) =
            if v >= 0 && v < ItemKind.all.Length then
                this.Kind <- ItemKind.all.[v]

    member _.KindLabel = I18n.kindLabel kind

    member _.Name
        with get () = name
        and set (v: string) =
            let v = if isNull v then "" else v
            if name <> v then
                beforeChange ()
                name <- v
                raise' "Name"

    member _.Device
        with get () = device
        and set (v: string) =
            let v = if isNull v then "" else v.Trim().ToUpperInvariant()
            if device <> v then
                beforeChange ()
                device <- v
                raise' "Device"

    member _.MonitorDevice
        with get () = monitor
        and set (v: string) =
            let v = if isNull v then "" else v.Trim().ToUpperInvariant()
            if monitor <> v then
                beforeChange ()
                monitor <- v
                raise' "MonitorDevice"

    member _.Action
        with get () = action
        and set (v: SwitchAction) =
            if action <> v then
                beforeChange ()
                action <- v
                raise' "Action"
                raise' "ActionIndex"
                raise' "ActionLabel"

    /// 콤보 상자용 (0..4)
    member this.ActionIndex
        with get () = SwitchAction.all |> List.findIndex (fun a -> a = action)
        and set (v: int) =
            if v >= 0 && v < SwitchAction.all.Length then
                this.Action <- SwitchAction.all.[v]

    member _.ActionLabel = I18n.actionLabel action

    member _.Min
        with get () = minValue
        and set (v: int) =
            if minValue <> v then
                beforeChange ()
                minValue <- v
                raise' "Min"

    member _.Max
        with get () = maxValue
        and set (v: int) =
            if maxValue <> v then
                beforeChange ()
                maxValue <- v
                raise' "Max"

    member _.X
        with get () = x
        and set (v: int) =
            let v = max 0 v
            if x <> v then
                beforeChange ()
                x <- v
                raise' "X"

    member _.Y
        with get () = y
        and set (v: int) =
            let v = max 0 v
            if y <> v then
                beforeChange ()
                y <- v
                raise' "Y"

    member _.Width
        with get () = width
        and set (v: int) =
            let v = max Limits.minWidth v
            if width <> v then
                beforeChange ()
                width <- v
                raise' "Width"

    member _.Height
        with get () = height
        and set (v: int) =
            let v = max Limits.minHeight v
            if height <> v then
                beforeChange ()
                height <- v
                raise' "Height"

    /// 캔버스 드래그/크기조절에서 한 번에 반영 (알림 1회로 묶음)
    member this.SetBounds(nx: int, ny: int, nw: int, nh: int) =
        this.X <- nx
        this.Y <- ny
        this.Width <- nw
        this.Height <- nh

    /// 이 요소의 마지막 통신 오류. Some 이면 운전 화면에서 빨간색으로 점등된다.
    member _.Fault
        with get () = fault
        and set (v: string option) = fault <- v

    member _.ToItem() : HmiItem =
        { Id = id
          Enabled = enabled
          Visible = visible
          Kind = kind
          Name = name
          Device = device
          MonitorDevice = monitor
          Action = action
          Min = minValue
          Max = maxValue
          X = x
          Y = y
          Width = width
          Height = height }

    /// 표에 보여줄 짧은 설명
    member this.Summary =
        let dev = if String.IsNullOrWhiteSpace device then "-" else device
        sprintf "%s  ·  %s" (if String.IsNullOrWhiteSpace name then "(no name)" else name) dev

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member _.PropertyChanged = propertyChanged.Publish
