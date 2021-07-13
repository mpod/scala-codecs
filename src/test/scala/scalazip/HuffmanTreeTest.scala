package scalazip

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.{BinStringSyntax, BitVector}

class HuffmanTreeTest extends AnyFlatSpec with Matchers {

  it should "aaa" in {
    val codec = scodec.codecs.list(
      HuffmanCodec(
        Seq(3, 3, 3, 3, 3, 2, 4, 4),
        Seq("A", "B", "C", "D", "E", "F", "G", "H")
      )
    )
    val x: BitVector = bin"0100111001011100011101111"
    println(codec.decode(x).require.value)

    val encoded = codec.encode(List("A", "B", "C", "D", "E", "F", "G", "H")).require.toBin
    println(encoded)
    println(encoded == x.toBin)

    val y: BitVector = bin"01100011 11000101 00001101"
    println(y.toBin)
//    println(y.reverseBitOrder.toBin)
    println(y.reverseByteOrder.toBin)
  }

}
