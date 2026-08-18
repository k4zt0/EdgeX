namespace XgbHmi.Protocol

open System

/// XGB / XGT 디바이스 주소 해석.
///
/// XGB의 P/M 비트 표기 규칙 (v6에서 확정된 규칙 그대로):
///   첫 글자는 디바이스 종류, 중간 자리는 10진수 WORD 위치, 마지막 한 자리는 16진수 비트 위치(0~F).
///   M01008 = MW100 의 bit 8  = XGT 직접변수 %MX1608
///   M0100F = MW100 의 bit F  = XGT 직접변수 %MX1615
///   P00120 = PW12  의 bit 0  = XGT 직접변수 %PX192
[<RequireQualifiedAccess>]
module Address =

    type BitAddress =
        { Area: char
          Word: int
          Bit: int }

    let parseBit (address: string) : BitAddress =
        let a = (if isNull address then "" else address.Trim().ToUpperInvariant())
        if a.Length < 2 then
            raise (ArgumentException("지원하지 않는 BIT 주소: " + address))

        let area = a.[0]
        if area <> 'P' && area <> 'M' then
            raise (ArgumentException("지원하는 BIT 영역은 P/M 입니다: " + address))

        let raw = a.Substring 1
        if raw.Length < 2 then
            raise (ArgumentException(string area + " BIT 주소가 잘못되었습니다: " + address))

        let bitChar = raw.[raw.Length - 1]
        let bit =
            if bitChar >= '0' && bitChar <= '9' then int bitChar - int '0'
            elif bitChar >= 'A' && bitChar <= 'F' then 10 + (int bitChar - int 'A')
            else raise (ArgumentException("P BIT 주소의 마지막 자리가 잘못되었습니다: " + address))

        let wordText = raw.Substring(0, raw.Length - 1)
        match Int32.TryParse wordText with
        | true, word -> { Area = area; Word = word; Bit = bit }
        | _ -> raise (ArgumentException("P BIT 주소의 WORD 부분이 잘못되었습니다: " + address))

    /// "%MX1608" 형태의 XGT 직접변수 이름
    let toXgtBit (address: string) =
        let b = parseBit address
        "%" + string b.Area + "X" + string (Checked.(*) b.Word 16 + b.Bit)

    /// "%MW100" 형태의 XGT 직접변수 이름
    let toXgtWord (area: char) (word: int) =
        "%" + string (Char.ToUpperInvariant area) + "W" + string word

    /// D200 -> 200 (WORD 단일 읽기/쓰기는 D 영역만 지원)
    let parseDWord (verb: string) (address: string) =
        let a = (if isNull address then "" else address.Trim().ToUpperInvariant())
        if a.Length < 2 || a.[0] <> 'D' then
            raise (ArgumentException("현재 WORD 단일 " + verb + "는 D영역만 지원합니다: " + address))
        match Int32.TryParse(a.Substring 1) with
        | true, w -> w
        | _ -> raise (ArgumentException("D 주소가 잘못되었습니다: " + address))
