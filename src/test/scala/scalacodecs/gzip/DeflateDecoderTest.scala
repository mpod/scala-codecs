package scalacodecs.gzip

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import fs2.Stream
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalacodecs.gzip.DeflateDecoder.{Literal, LiteralBlock}
import scodec.Decoder
import scodec.bits.{BitVector, HexStringSyntax}

class DeflateDecoderTest extends AnyFlatSpec with Matchers {

  it should "decode multi-block byte array" in {
    // Following byte array is manually crafted. It contains 3 blocks:
    // 1. non-compressed block
    // 2. block compressed with dynamic Huffman codes
    // 3. block compressed with fixed Huffman codes
    // All three blocks decode to the same value: 0x616263 (or string 'abc'). With the help of DeflateDecoder.log it is
    // possible to find boundaries between those blocks. Deflate algorithm reads bits in a byte from right to left.

    val bytes =
      hex"""
      1F 8B 08 08 CC D7 ED 60 00 03 61 62 63 00 EC 5A 7F 6C 13 D7 1D 7F 67 E7 17 24 C4 A6 A5 2D 04 4A DC 35 54 A4
      5D 5C 42 28 0D 65 6D 6D 82 E1 CC 1C F0 68 52 58 07 5C 1C DB 49 3C 1C 3B B5 EF 68 D2 02 8D 6A DA 72 72 43 3B
      09 6D D9 50 B5 48 53 7F 4C 43 22 7F B4 8C B6 DA EA 00 23 29 12 53 C2 D4 96 75 EB 96 76 C0 92 00 6A A6 A6 D4
      ED 20 DE F7 DD BD 77 BE 3B DB B4 D2 A4 49 93 F2 B5 EC EF 7D 3F EF FB FD BE EF FB F5 7D 77 BE E7 69 F6 FE 47
      89 49 C9 80 00 03 00 FC FF 61 62 63 18 48 2D 46 09 00 00 00
      """
    val decoder = Decoder[GzipFile]

    decoder.decode(bytes.toBitVector).require.value.data shouldBe hex"61 62 63 61 62 63 61 62 63"
  }

  it should "stream decoded multi-block byte array" in {
    val bytes =
      hex"""
      1F 8B 08 08 CC D7 ED 60 00 03 61 62 63 00 EC 5A 7F 6C 13 D7 1D 7F 67 E7 17 24 C4 A6 A5 2D 04 4A DC 35 54 A4
      5D 5C 42 28 0D 65 6D 6D 82 E1 CC 1C F0 68 52 58 07 5C 1C DB 49 3C 1C 3B B5 EF 68 D2 02 8D 6A DA 72 72 43 3B
      09 6D D9 50 B5 48 53 7F 4C 43 22 7F B4 8C B6 DA EA 00 23 29 12 53 C2 D4 96 75 EB 96 76 C0 92 00 6A A6 A6 D4
      ED 20 DE F7 DD BD 77 BE 3B DB B4 D2 A4 49 93 F2 B5 EC EF 7D 3F EF FB FD BE EF FB F5 7D 77 BE E7 69 F6 FE 47
      89 49 C9 80 00 03 00 FC FF 61 62 63 18 48 2D 46 09 00 00 00
      """
    val res = Stream
      .emits(bytes.toArray.map(BitVector.apply(_)))
      .covary[IO]
      .through(StreamGzipFileDecoder.toPipe)
      .compile
      .toList
      .unsafeRunSync()

    res shouldBe List(Literal(97),
                      Literal(98),
                      Literal(99),
                      Literal(97),
                      Literal(98),
                      Literal(99),
                      LiteralBlock(hex"616263")
    )
  }

}
