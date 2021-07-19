package scalazip

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

import scala.annotation.tailrec

sealed trait HuffmanTree[+A]
case object Empty                                                extends HuffmanTree[Nothing]
case class Symbol[+A](symbol: A)                                 extends HuffmanTree[A]
case class Branch[+A](zero: HuffmanTree[A], one: HuffmanTree[A]) extends HuffmanTree[A]

object HuffmanTree {
  def add[A, B >: A](tree: HuffmanTree[A], bits: BitVector, symbol: B): Option[HuffmanTree[B]] =
    tree match {
      case Empty if bits.isEmpty                           => Some(Symbol(symbol))
      case Empty if !bits.head                             => add(tree, bits.tail, symbol).map(Branch(_, Empty))
      case Empty                                           => add(tree, bits.tail, symbol).map(Branch(Empty, _))
      case tree @ Symbol(s) if bits.isEmpty && s == symbol => Some(tree)
      case Symbol(_)                                       => None
      case Branch(_, _) if bits.isEmpty                    => None
      case Branch(zero, one) if !bits.head                 => add(zero, bits.tail, symbol).map(Branch(_, one))
      case Branch(zero, one)                               => add(one, bits.tail, symbol).map(Branch(zero, _))
    }

  def height(tree: HuffmanTree[_]): Int =
    tree match {
      case Empty             => 0
      case Symbol(_)         => 0
      case Branch(zero, one) => 1 + math.max(height(zero), height(one))
    }

  def symbolToCodeMap[A](tree: HuffmanTree[A]): Map[A, BitVector] =
    tree match {
      case Empty     => Map.empty
      case Symbol(s) => Map(s -> BitVector.empty)
      case Branch(zero, one) =>
        val zeroMap = symbolToCodeMap(zero).map { case (symbol, bits) => symbol -> (false +: bits) }
        val oneMap  = symbolToCodeMap(one).map { case (symbol, bits) => symbol -> (true +: bits) }
        zeroMap ++ oneMap
    }
}

final class HuffmanCodec[A](tree: HuffmanTree[A]) extends Codec[A] {
  override def decode(bits: BitVector): Attempt[DecodeResult[A]] =
    runDecoding(bits, tree)

  @tailrec
  private def runDecoding(buffer: BitVector, tree: HuffmanTree[A]): Attempt[DecodeResult[A]] =
    tree match {
      case Symbol(symbol)                => Attempt.successful(DecodeResult(symbol, buffer))
      case _ if buffer.isEmpty           => Attempt.failure(Err.InsufficientBits(1L, buffer.size, Nil))
      case Empty                         => Attempt.failure(Err.General("Invalid input.", Nil))
      case Branch(_, one) if buffer.head => runDecoding(buffer.tail, one)
      case Branch(zero, _)               => runDecoding(buffer.tail, zero)
    }

  private lazy val symbolToCode = HuffmanTree.symbolToCodeMap(tree)

  override def encode(value: A): Attempt[BitVector] =
    if (symbolToCode.contains(value))
      Attempt.successful(symbolToCode(value))
    else
      Attempt.failure(Err.General(s"Cannot encode symbol $value", Nil))

  override def sizeBound: SizeBound = SizeBound.bounded(1, HuffmanTree.height(tree))
}

object HuffmanCodec {

  def apply(lengths: Seq[Int]): Option[Codec[Int]] = apply(lengths, lengths.indices)

  def apply[A](lengths: Seq[Int], symbols: Seq[A]): Option[Codec[A]] = {
    require(lengths.nonEmpty)
    require(lengths.length == symbols.length)

    val bitLengthCount =
      lengths
        .foldLeft(Map.empty[Int, Int]) { case (m, l) =>
          m.updated(l, m.getOrElse(l, 0) + 1)
        }
        .withDefaultValue(0)
    val nextCode =
      (1 to lengths.max)
        .foldLeft(Map(0 -> 0)) { case (m, l) =>
          val code = m(l - 1)
          m.updated(l, (code + bitLengthCount(l - 1)) << 1)
        }
    val (_, tree) =
      lengths
        .zip(symbols)
        .foldLeft((nextCode, Option(Empty: HuffmanTree[A]))) {
          case (acc, (0, _)) => acc
          case ((nextCode, tree), (codeLength, symbol)) =>
            val bits = BitVector.fromInt(nextCode(codeLength), codeLength)
            (
              nextCode.updated(codeLength, nextCode(codeLength) + 1),
              tree.flatMap(HuffmanTree.add(_, bits, symbol))
            )
        }

    tree.map(new HuffmanCodec(_))
  }

  def interpolated(pairs: Seq[(Int, Int)]): Option[Codec[Int]] = {
    require(pairs.nonEmpty)
    val (lastLength, lastSymbol) = pairs.last
    val (lengths, symbols) = pairs
      .sliding(2)
      .foldRight((List(lastLength), List(lastSymbol))) {
        case ((l1, s1) :: (_, s2) :: Nil, (lengths, symbols)) =>
          val n = s2 - s1
          (List.fill(n)(l1) ::: lengths, (s1 until s2).toList ::: symbols)
        case (_ :: Nil, acc) => acc
      }
    HuffmanCodec(lengths, symbols)
  }
}
