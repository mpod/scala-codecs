package scalacodecs.gzip

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scodec.bits.BinStringSyntax
import scodec.{Codec, Err}

class HuffmanTreeTest extends AnyFlatSpec with Matchers {

  private def fold[A](codec: Option[Codec[A]]): Codec[A] =
    codec.fold(scodec.codecs.fail[A](Err.General("Failed to create codec.", Nil)))(identity)

  it should "decode example from RFC" in {
    val codec = scodec.codecs.list(
      fold(
        HuffmanCodec(
          Seq(3, 3, 3, 3, 3, 2, 4, 4),
          Seq("A", "B", "C", "D", "E", "F", "G", "H")
        )
      )
    )
    codec.decode(bin"0100111001011100011101111").require.value shouldBe List("A", "B", "C", "D", "E", "F", "G", "H")
  }

  it should "encode example from RFC" in {
    val codec = scodec.codecs.list(
      fold(
        HuffmanCodec(
          Seq(3, 3, 3, 3, 3, 2, 4, 4),
          Seq("A", "B", "C", "D", "E", "F", "G", "H")
        )
      )
    )
    codec.encode(List("A", "B", "C", "D", "E", "F", "G", "H")).require.toBin shouldBe "0100111001011100011101111"
  }

}
