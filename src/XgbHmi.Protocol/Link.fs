namespace XgbHmi.Protocol

open System
open System.Collections.Generic
open System.Threading

/// PLC가 되돌려 준 오류 코드를 그대로 담는다. (FEnet ErrorStatus / Cnet NAK 코드)
exception XgtProtocolException of message: string * code: int

/// 통신 추적 한 줄. 화면 출력 창에서 실제 오간 내용을 그대로 보여 주기 위한 것.
type XgtTraceKind =
    | Tx
    | Rx
    | Note

type XgtTrace =
    { Kind: XgtTraceKind
      /// 사람이 읽는 요약 (예: "READ WORD %MW100 x1")
      Summary: string
      /// 실제 바이트 (추적을 켰을 때만 채운다)
      Hex: string
      ElapsedMs: float }

/// 이더넷(FEnet) 이든 RS-232C / RS-485(Cnet) 이든 화면 쪽에서는 이 창구 하나로만 쓴다.
/// 덕분에 폴링·쓰기·추적 코드는 연결 방식을 몰라도 된다.
type IPlcLink =
    inherit IDisposable
    /// 연결하고 통신이 실제로 되는지 시험 프레임까지 주고받는다.
    abstract Connect: unit -> unit
    abstract Connected: bool
    /// 자동으로 고른 통신 조합 이름 (출력 창/상태 표시줄에 그대로 보여 준다)
    abstract ProfileName: string
    /// 연결할 때 어떤 조합을 시험했는지 남긴 기록
    abstract NegotiationLog: string
    abstract FrameCount: int64
    abstract ErrorCount: int64
    abstract TraceEnabled: bool with get, set
    abstract Trace: IEvent<XgtTrace>
    abstract ReadBits: IList<string> -> Dictionary<string, bool>
    abstract ReadWord: string -> uint16
    abstract WriteBit: string * bool -> unit
    abstract WriteWord: string * uint16 -> unit

/// M 비트 쓰기 규칙. 이더넷과 직렬이 똑같이 따라야 해서 한 곳에 둔다.
[<RequireQualifiedAccess>]
module MBit =

    /// XGB에서 M 비트 ON/OFF를 확실하게 처리하기 위해
    /// %MX 직접 쓰기 대신 해당 %MW를 읽고 그 비트만 바꿔 WORD로 되쓴다. (v5 TOGGLE FIX)
    /// M01008 -> MW100 bit8, M01009 -> MW100 bit9, M0100F -> MW100 bit15 ...
    /// readWord/writeWord 는 M 영역 WORD 를 읽고 쓰는 함수, note 는 추적 한 줄을 남기는 함수다.
    let writeByWord
        (readWord: int -> uint16 option)
        (writeWord: int -> uint16 -> unit)
        (note: string -> unit)
        (address: string)
        (value: bool)
        =
        let b = Address.parseBit address
        if b.Area <> 'M' then raise (ArgumentException("M BIT 전용 함수입니다: " + address))

        let mask = uint16 (1 <<< b.Bit)
        let mutable lastRead = 0us
        let mutable finished = false
        let mutable attempt = 0

        while not finished && attempt < 3 do
            let before =
                match readWord b.Word with
                | Some v -> v
                | None -> raise (IO.IOException(address + " 쓰기 전 MW" + string b.Word + " 읽기 실패"))

            let changed = if value then before ||| mask else before &&& (~~~mask)
            note (
                sprintf
                    "%s RMW #%d: %s 0x%04X -> 0x%04X (bit%d %s)"
                    address
                    (attempt + 1)
                    (Address.toXgtWord 'M' b.Word)
                    before
                    changed
                    b.Bit
                    (if value then "SET" else "CLEAR")
            )
            writeWord b.Word changed
            Thread.Sleep 20

            let after =
                match readWord b.Word with
                | Some v -> v
                | None -> raise (IO.IOException(address + " 쓰기 후 MW" + string b.Word + " 읽기 실패"))

            lastRead <- after
            note (
                sprintf
                    "%s READBACK #%d: %s = 0x%04X -> bit%d %s"
                    address
                    (attempt + 1)
                    (Address.toXgtWord 'M' b.Word)
                    after
                    b.Bit
                    (if (after &&& mask) <> 0us then "ON" else "OFF")
            )
            if ((after &&& mask) <> 0us) = value then finished <- true
            else
                Thread.Sleep 20
                attempt <- attempt + 1

        if not finished then
            let lastState = (lastRead &&& mask) <> 0us
            raise (
                IO.IOException(
                    address
                    + " "
                    + (if value then "ON" else "OFF")
                    + " 쓰기 후에도 실제 비트가 "
                    + (if lastState then "ON" else "OFF")
                    + "입니다. PLC 래더에서 같은 M비트를 다시 쓰고 있는지 확인하십시오."
                )
            )
