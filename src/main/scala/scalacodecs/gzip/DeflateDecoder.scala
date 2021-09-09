package scalacodecs.gzip

import scodec.Attempt.{Failure, Successful}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.{bits, _}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}

import scala.collection.mutable.ListBuffer
import scala.language.implicitConversions

class DeflateDecoder extends Decoder[ByteVector] {

  import DeflateDecoder._

  private def collectInternalSymbols(dec: Decoder[InternalSymbol]): Decoder[List[InternalSymbol]] =
    Decoder(
      decodeCollectWhile(
        dec,
        ListBuffer.empty[InternalSymbol]
      ) { case (a, acc) =>
        Attempt.successful(acc += a)
      } { case (a, _) =>
        !a.contains(EndOfBlock)
      }
    ).map(_.result())

  def log[A](decoder: Decoder[A], prefix: String = ""): Codec[A] = {
    val pfx = if (prefix.isEmpty) "" else s"$prefix: "
    logSuccessesBuilder[A](
      (_, _) => (),
      (_, r) => {
        val s    = r.remainder.size
        val r1   = r.remainder.take(s % 8).reverse.toBin
        val r2   = r.remainder.drop(s % 8).reverseBitOrder.take(4 * 8).toHex
        val dots = "." * (8 - r1.length)
        println(s"${pfx}decoded ${r.value}, remaining 0b$r1$dots:0x$r2...")
      }
    )(decoder.decodeOnly)
  }

  private val block: Decoder[List[InternalSymbol]] =
    for {
      lastBlock <- bool
      blockType <- uint(2)
      block <-
        if (blockType == 0)
          nonCompressedDecoderArgs.flatMap(nonCompressedDecoder).map(List(_))
        else if (blockType == 1)
          collectInternalSymbols(fixedHuffmanDecoder)
        else if (blockType == 2)
          dynamicHuffmanDecoderArgs.flatMap(args => collectInternalSymbols(dynamicHuffmanDecoder(args)))
        else
          fail(Err.General("Invalid block type.", Nil))
      _ <- conditional(lastBlock, Decoder.modify(bits => bits.drop(bits.size % 8).reverseBitOrder).decodeOnly)
    } yield if (lastBlock) block :+ EndOfLastBlock else block

  override def decode(bits: BitVector): Attempt[DecodeResult[ByteVector]] =
    decodeCollectWhile(
      block,
      ByteVector.empty.buffer
    ) { case (internalSymbols, acc) =>
      internalSymbols.foldLeft(Attempt.successful(acc)) {
        case (acc, Literal(v)) =>
          acc.map(_ :+ v)
        case (Successful(out), LengthDistancePair(_, distance)) if out.length < distance =>
          Attempt.failure(Err.General("Distance value is larger than the size of the output stream.", Nil))
        case (acc, LengthDistancePair(length, distance)) =>
          (0 until length).foldLeft(acc) { case (acc, _) =>
            acc.map(out => out :+ out(out.length - distance))
          }
        case (acc, LiteralBlock(data)) =>
          acc.map(_ ++ data)
        case (acc, _) =>
          acc
      }
    } { case (a, _) =>
      !a.flatMap(_.lastOption).contains(EndOfLastBlock)
    }(bits.reverseBitOrder).map(_.mapRemainder(_.reverseBitOrder))
}

object DeflateDecoder {
  sealed private[gzip] trait InternalSymbol
  private[gzip] case object EndOfBlock                                    extends InternalSymbol
  private[gzip] case object EndOfLastBlock                                extends InternalSymbol
  private[gzip] case class Literal(value: Byte)                           extends InternalSymbol
  private[gzip] case class LengthDistancePair(length: Int, distance: Int) extends InternalSymbol
  private[gzip] case class LiteralBlock(data: ByteVector)                 extends InternalSymbol

  private[gzip] case class BlockHeader(lastBlock: Boolean, blockType: Int)
  private[gzip] case class NonCompressedDecoderArgs(n: Int)
  private[gzip] case class DynamicHuffmanDecoderArgs(litLengths: List[Int], distLengths: List[Int])

  implicit private def fold[A](codec: Option[Codec[A]]): Codec[A] =
    codec.fold(fail[A](Err.General("Failed to create codec.", Nil)))(identity)

  // format: off
  private val lengthBase: Array[Int] = Array( 3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31, 35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258 )
  private val distanceBase: Array[Int] = Array(1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193, 257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577)
  // format: on

  private val uint: Array[Decoder[Int]] =
    (0 to 16).map(i => bits(i).map(_.reverse.toInt(signed = false))).toArray

  private[gzip] val blockHeader: Decoder[BlockHeader] =
    for {
      lastBlock <- bool
      blockType <- uint(2)
      _         <- conditional[Unit](blockType < 0 || blockType >= 3, fail(Err.General("Invalid block type.", Nil)))
    } yield BlockHeader(lastBlock, blockType)

  private[gzip] val nonCompressedDecoderArgs: Decoder[NonCompressedDecoderArgs] =
    for {
      _    <- Decoder.modify(bits => bits.drop(bits.size % 8))
      len  <- uint(16)
      nlen <- uint(16)
      _    <- conditional[Unit]((len & nlen) != 0, fail(Err.General("Invalid non-compressed block.", Nil)))
    } yield NonCompressedDecoderArgs(len)

  private[gzip] def nonCompressedDecoder(args: NonCompressedDecoderArgs): Decoder[InternalSymbol] =
    bytes(args.n).map(_.map(BitVector.reverseBitsInByte)).map(LiteralBlock)

  private[gzip] val fixedHuffmanDecoder: Decoder[InternalSymbol] = {
    val literalCodec  = HuffmanCodec.interpolated(List((8, 0), (9, 144), (7, 256), (8, 280), (8, 287)))
    val distanceCodec = HuffmanCodec.interpolated(List((5, 0), (5, 29)))
    internalSymbol(literalCodec, distanceCodec)
  }

  private[gzip] val dynamicHuffmanDecoderArgs: Decoder[DynamicHuffmanDecoderArgs] =
    for {
      hlit        <- uint(5).map(_ + 257)
      hdist       <- uint(5).map(_ + 1)
      hclen       <- uint(4).map(_ + 4)
      codeLengths <- vectorOfN(provide(hclen), uint(3).decodeOnly).map(orderLengths)
      lengthCodec = HuffmanCodec(codeLengths)
      litLengths  <- lengthSequence(hlit, lengthCodec)
      distLengths <- lengthSequence(hdist, lengthCodec)
    } yield DynamicHuffmanDecoderArgs(litLengths, distLengths)

  private[gzip] def dynamicHuffmanDecoder(args: DynamicHuffmanDecoderArgs): Decoder[InternalSymbol] = {
    val literalCodec  = HuffmanCodec(args.litLengths)
    val distanceCodec = HuffmanCodec(args.distLengths)
    internalSymbol(literalCodec, distanceCodec)
  }

  private def orderLengths(v: Vector[Int]): Vector[Int] =
    v.padTo(19, 0)
      .zip(Vector(16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15))
      .sortBy(_._2)
      .map(_._1)

  private def lengthExtraBits(a: Int): Decoder[Int] = {
    val n = if ((a >= 257 && a < 261) || (a == 285)) 0 else (a - 261) / 4
    uint(n)
  }

  private def distanceExtraBits(a: Int): Decoder[Int] = {
    val n = if ((a == 0) || (a == 1)) 0 else (a - 2) / 2
    uint(n)
  }

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
      case symbol => fail(Err.General(s"Invalid internal symbol $symbol.", Nil))
    }

  private def lengthSequence(count: Int, codec: Codec[Int]): Decoder[List[Int]] =
    Decoder(
      decodeCollectWhile(
        codec.flatMap { a =>
          if (a >= 0 && a <= 15) provide((a, 0))
          else if (a == 16) uint(2).map(b => (a, b + 3))
          else if (a == 17) uint(3).map(b => (a, b + 3))
          else if (a == 18) uint(7).map(b => (a, b + 11))
          else fail(Err.General(s"Invalid code length code $a.", Nil))
        },
        ListBuffer.empty[Int]
      ) {
        case ((a, 0), acc) =>
          Attempt.successful(acc += a)
        case ((16, _), acc) if acc.isEmpty =>
          Attempt.failure(Err.General("Invalid use of code length code 16.", Nil))
        case ((16, b), acc) =>
          Attempt.successful(acc ++= List.fill(b)(acc.last))
        case ((17 | 18, b), acc) =>
          Attempt.successful(acc ++= List.fill(b)(0))
      } { case (_, b) =>
        b.length < count
      }
    ).map(_.result())

  private def decodeCollectWhile[A, B](
    dec: Decoder[A],
    zero: B
  )(
    append: (A, B) => Attempt[B]
  )(
    condition: (Option[A], B) => Boolean
  ): BitVector => Attempt[DecodeResult[B]] =
    (bits: BitVector) => {
      var remaining       = bits
      var acc             = Attempt.successful(zero)
      var last: Option[A] = None
      while (acc.toOption.exists(b => condition(last, b)))
        dec.decode(remaining) match {
          case Attempt.Successful(DecodeResult(value, rest)) =>
            acc = acc.flatMap(b => append(value, b))
            last = Some(value)
            remaining = rest
          case failure: Failure =>
            acc = failure
            remaining = BitVector.empty
        }
      acc.map(DecodeResult(_, remaining))
    }
}
