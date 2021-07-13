package scalazip

import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}

import scala.collection.mutable.ListBuffer
import scala.language.higherKinds

class DeflateDecoder extends Decoder[ByteVector] {

  sealed trait InternalSymbol
  case object EndOfBlock                                    extends InternalSymbol
  case object EndOfLastBlock                                extends InternalSymbol
  case class Literal(value: Byte)                           extends InternalSymbol
  case class LengthDistancePair(length: Int, distance: Int) extends InternalSymbol

  private val uint: Array[Decoder[Int]] =
    (0 to 16).map(i => bits(i).asDecoder.map(_.reverse.toInt(signed = false))).toArray

  private val nonCompressedBlock: Decoder[List[InternalSymbol]] =
    for {
      _    <- byteAligned(provide(()))
      len  <- uint(16)
      nlen <- uint(16)
      _    <- if ((len & nlen) == 0) provide(()) else fail(Err.General("Invalid input.", Nil))
      data <- bytes(len)
    } yield data.toSeq.map(Literal).toList

  private val fixedHuffmanBlock: Decoder[List[InternalSymbol]] = {
    val literalCodec  = HuffmanCodec.interpolated(List((8, 0), (9, 144), (7, 256), (8, 280), (8, 287)))
    val distanceCodec = HuffmanCodec.interpolated(List((5, 0), (5, 29)))
    val dec           = internalSymbol(literalCodec, distanceCodec)
    collectInternalSymbols(dec)
  }

  private def orderLengths(v: Vector[Int]): Vector[Int] =
    v.padTo(19, 0)
      .zip(Vector(16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15))
      .sortBy(_._2)
      .map(_._1)

  private val dynamicHuffmanBlock: Decoder[List[InternalSymbol]] =
    for {
      hlit        <- uint(5).map(_ + 257)
      hdist       <- uint(5).map(_ + 1)
      hclen       <- uint(4).map(_ + 4)
      codeLengths <- vectorOfN(provide(hclen), uint(3).decodeOnly).map(orderLengths)
      lengthCodec = HuffmanCodec(codeLengths)
      litLengths  <- lengthSequence(hlit, lengthCodec)
      distLengths <- lengthSequence(hdist, lengthCodec)
      litCodec  = HuffmanCodec(litLengths)
      distCodec = HuffmanCodec(distLengths)
      dec       = internalSymbol(litCodec, distCodec)
      symbols <- collectInternalSymbols(dec)
    } yield symbols

  private def lengthSequence(count: Int, codec: Codec[Int]): Decoder[List[Int]] =
    Decoder(
      decodeCollectWhile(
        codec.flatMap { a =>
          if (a >= 0 && a <= 15) provide((a, 0))
          else if (a == 16) uint(2).map(b => (a, b + 3))
          else if (a == 17) uint(3).map(b => (a, b + 3))
          else if (a == 18) uint(7).map(b => (a, b + 11))
          else fail(Err.General("Invalid code.", Nil))
        }.asDecoder,
        ListBuffer.empty[Int]
      ) {
        case ((a, 0), acc) =>
          acc += a
        case ((16, b), acc) if acc.nonEmpty =>
          acc ++= List.fill(b)(acc.last)
        case ((17 | 18, b), acc) =>
          acc ++= List.fill(b)(0)
      } { case (_, b) => b.length < count }
    ).map(_.result())

  private val lengthBase: Array[Int] =
    // format: off
    Array( 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 )
    // format: on

  private val distanceBase: Array[Int] =
    // format: off
    Array(1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577)
    // format: on

  private def internalSymbol(
    literalLengthDecoder: Decoder[Int],
    distanceDecoder: Decoder[Int]
  ): Decoder[InternalSymbol] =
    literalLengthDecoder.flatMap {
      case literal if literal >= 0 && literal < 256 => provide(Literal(literal.toByte))
      case 256                                      => provide(EndOfBlock)
      case lengthCode if lengthCode >= 257 && lengthCode < 286 =>
        for {
          lengthExtra   <- lengthExtraBits(lengthCode)
          distanceCode  <- distanceDecoder
          distanceExtra <- distanceExtraBits(distanceCode)
          lengthSymbol   = lengthBase(lengthCode - 257) + lengthExtra
          distanceSymbol = distanceBase(distanceCode) + distanceExtra
        } yield LengthDistancePair(lengthSymbol, distanceSymbol)
      case _ => fail(Err.General("Invalid code.", Nil))
    }

  private def lengthExtraBits(a: Int): Decoder[Int] = {
    val n = if ((a >= 257 && a < 261) || (a == 285)) 0 else (a - 261) / 4
    uint(n)
  }

  private def distanceExtraBits(a: Int): Decoder[Int] = {
    val n = if ((a == 0) || (a == 1)) 0 else (a - 2) / 2
    uint(n)
  }

  private def decodeCollectWhile[A, B](
    dec: Decoder[A],
    zero: B
  )(
    append: (A, B) => B
  )(
    condition: (Option[A], B) => Boolean
  ): BitVector => Attempt[DecodeResult[B]] =
    (bits: BitVector) => {
      var remaining          = bits
      var acc                = zero
      var last: Option[A]    = None
      var error: Option[Err] = None
      while (condition(last, acc) && remaining.nonEmpty)
        dec.decode(remaining) match {
          case Attempt.Successful(DecodeResult(value, rest)) =>
            acc = append(value, acc)
            last = Some(value)
            remaining = rest
          case Attempt.Failure(err) =>
            error = Some(err)
            remaining = BitVector.empty
        }
      Attempt.fromErrOption(error, DecodeResult(acc, remaining))
    }

  def collectInternalSymbols(dec: Decoder[InternalSymbol]): Decoder[List[InternalSymbol]] =
    Decoder(
      decodeCollectWhile(dec, ListBuffer.empty[InternalSymbol]) { case (a, acc) =>
        acc += a
      } { case (a, _) =>
        !a.contains(EndOfBlock)
      }
    ).map(_.result())

  private val block: Decoder[List[InternalSymbol]] =
    for {
      lastBlock <- bool
      blockType <- uint(2)
      block <-
        if (blockType == 0) nonCompressedBlock
        else if (blockType == 1) fixedHuffmanBlock
        else if (blockType == 2) dynamicHuffmanBlock
        else fail(Err.General("Invalid block type.", Nil))
      _ <-
        conditional(
          lastBlock,
          byteAligned(provide(())).asDecoder.flatMap(_ => Decoder.modify(_.reverseBitOrder)).decodeOnly
        )
    } yield if (lastBlock) block :+ EndOfLastBlock else block

  override def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] = {
    val dec = Decoder.modify(_.reverseBitOrder).flatMap(_ => block)
    decodeCollectWhile(dec, ByteVector.empty.buffer) { case (internalSymbols, acc) =>
      internalSymbols.foldLeft(acc) {
        case (acc, Literal(v)) =>
          acc :+ v
        case (acc, LengthDistancePair(length, distance)) =>
          (0 until length).foldLeft(acc) { case (acc, _) =>
            acc :+ acc(acc.length - distance)
          }
        case (acc, _) =>
          acc
      }
    } { case (a, _) =>
      !a.flatMap(_.lastOption).contains(EndOfLastBlock)
    }(bits)
  }
}
