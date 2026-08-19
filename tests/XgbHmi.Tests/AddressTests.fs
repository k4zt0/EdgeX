module XgbHmi.Tests.AddressTests

open System
open Xunit
open XgbHmi.Protocol

/// v6에서 확정된 XG5000 표기(10진 WORD + 16진 BIT)를 그대로 지키는지 확인한다.
[<Theory>]
[<InlineData("M01008", 'M', 100, 8, "%MX1608")>]
[<InlineData("M0100F", 'M', 100, 15, "%MX1615")>]
[<InlineData("M01009", 'M', 100, 9, "%MX1609")>]
[<InlineData("P00120", 'P', 12, 0, "%PX192")>]
[<InlineData("P0013F", 'P', 13, 15, "%PX223")>]
[<InlineData("M1000", 'M', 100, 0, "%MX1600")>]
let ``비트 주소를 XG5000 표기대로 해석한다`` (address: string, area: char, word: int, bit: int, xgt: string) =
    let parsed = Address.parseBit address
    Assert.Equal(area, parsed.Area)
    Assert.Equal(word, parsed.Word)
    Assert.Equal(bit, parsed.Bit)
    Assert.Equal(xgt, Address.toXgtBit address)

[<Theory>]
[<InlineData("")>]
[<InlineData("X1000")>]
[<InlineData("D200")>]
[<InlineData("M")>]
[<InlineData("M100G")>]
let ``잘못된 비트 주소는 예외를 낸다`` (address: string) =
    Assert.ThrowsAny<exn>(fun () -> Address.parseBit address |> ignore)

[<Fact>]
let ``WORD 직접변수 이름을 만든다`` () =
    Assert.Equal("%MW100", Address.toXgtWord 'M' 100)
    Assert.Equal("%DW200", Address.toXgtWord 'd' 200)
    Assert.Equal("%PW12", Address.toXgtWord 'P' 12)

[<Fact>]
let ``D 주소만 WORD 단일 읽기 쓰기를 허용한다`` () =
    Assert.Equal(200, Address.parseDWord "읽기" "D200")
    Assert.Equal(100, Address.parseDWord "쓰기" " d100 ")
    Assert.ThrowsAny<exn>(fun () -> Address.parseDWord "읽기" "M100" |> ignore)

[<Fact>]
let ``formatBit 은 parseBit 의 반대다`` () =
    Assert.Equal("P00120", Address.formatBit 'P' 12 0)
    Assert.Equal("M01008", Address.formatBit 'M' 100 8)
    Assert.Equal("M0100F", Address.formatBit 'M' 100 15)
    Assert.Equal("M00510", Address.formatBit 'M' 51 0)
    for address in [ "M01008"; "P00120"; "M0100F"; "P0013F"; "M00501" ] do
        let b = Address.parseBit address
        Assert.Equal(address, Address.formatBit b.Area b.Word b.Bit)

[<Fact>]
let ``offsetBit 은 WORD 경계를 넘어간다`` () =
    Assert.Equal("P00121", Address.offsetBit "P00120" 1)
    Assert.Equal("P00127", Address.offsetBit "P00120" 7)
    Assert.Equal("P0012F", Address.offsetBit "P00120" 15)
    // PW12 bit15 다음은 PW13 bit0
    Assert.Equal("P00130", Address.offsetBit "P00120" 16)
    Assert.Equal("M01008", Address.offsetBit "M01000" 8)
    Assert.Equal("M01010", Address.offsetBit "M0100F" 1)
