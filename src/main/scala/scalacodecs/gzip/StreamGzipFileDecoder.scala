package scalacodecs.gzip

import fs2.{Chunk, Pipe, Pull, RaiseThrowable, Stream}
import scalacodecs.gzip.DeflateDecoder.{EndOfLastBlock, InternalSymbol, LengthDistancePair, Literal, LiteralBlock}
import scodec.Decoder
import scodec.bits.{BitVector, ByteVector}
import scodec.stream.StreamDecoder

import java.nio.ByteBuffer
import scala.collection.mutable.ArrayBuffer

object StreamGzipFileDecoder {
  val decoder: StreamDecoder[InternalSymbol] = {
    val header = StreamDecoder
      .once(Decoder.modify(_.reverseBitOrder).flatMap(_ => GzipFile.header))
      .flatMap(_ => StreamDecoder.empty)
    val body = StreamDeflateDecoder.decoder.flatMap { symbol =>
      if (symbol == EndOfLastBlock)
        StreamDecoder.once(GzipFile.tail).flatMap(_ => StreamDecoder.empty)
      else
        StreamDecoder.emit(symbol)
    }
    header ++ body
  }

  def toPipe[F[_]: RaiseThrowable]: Pipe[F, BitVector, BitVector] =
    in => in.map(_.reverseBitOrder).through(decoder.toPipe).through(decodeInternalSymbols[F])

  class Buffer {
    private val capacity: Int                      = 32 * 1024
    private val internalBuffers: Array[ByteBuffer] = Array.fill(2)(allocate)
    private var i: Int                             = 0

    private def allocate: ByteBuffer = ByteBuffer.allocate(capacity)

    def addOne(b: Byte): Chunk[BitVector] = {
      internalBuffers(i / capacity).put(b)
      i = (i + 1) % (2 * capacity)
      if (i == 0) {
        val first = internalBuffers(0)
        internalBuffers(0) = internalBuffers(1)
        internalBuffers(1) = allocate
        Chunk(BitVector(first.array()))
      } else
        Chunk.empty[BitVector]
    }

    def addAll(bytes: ByteVector): Chunk[BitVector] =
      Chunk.buffer(
        bytes.foldLeft(ArrayBuffer.empty[BitVector]) { (acc, b) =>
          addOne(b).foreach(b => acc += b)
          acc
        }
      )

    def getRelative(dist: Int): Option[Byte] = {
      val ii = i - dist
      if (ii < 0) None
      else Some(internalBuffers(ii / capacity).get(ii % capacity))
    }

    def drain: Chunk[BitVector] =
      if (i / capacity == 0)
        Chunk(BitVector(internalBuffers(0).array()).take(i * 8))
      else
        Chunk(BitVector(internalBuffers(0).array()), BitVector(internalBuffers(1).array()).take(i % capacity))

    def length: Int = i
  }

  def decodeInternalSymbols[F[_]: RaiseThrowable](s: Stream[F, InternalSymbol]): Stream[F, BitVector] = {
    def go(
      s: Stream[F, InternalSymbol],
      buffer: Buffer
    ): Pull[F, BitVector, Unit] =
      s.pull.uncons1
        .flatMap {
          case None =>
            val chunk = buffer.drain
            if (chunk.isEmpty) Pull.done
            else Pull.output(chunk)
          case Some((Literal(symbol), tl)) =>
            val chunk = buffer.addOne(symbol)
            if (chunk.isEmpty) go(tl, buffer)
            else Pull.output(chunk) >> go(tl, buffer)
          case Some((LiteralBlock(symbols), tl)) =>
            val chunk = buffer.addAll(symbols)
            if (chunk.isEmpty) go(tl, buffer)
            else Pull.output(chunk) >> go(tl, buffer)
          case Some((LengthDistancePair(_, dist), _)) if dist > buffer.length =>
            Pull.raiseError[F](new Exception("Distance value is larger than the size of the output stream."))
          case Some((LengthDistancePair(length, dist), tl)) =>
            val out = (0 until length).foldLeft(ArrayBuffer.empty[BitVector]) { case (acc, _) =>
              buffer.getRelative(dist).map(b => buffer.addOne(b)).foreach(_.foreach(acc += _))
              acc
            }
            Pull.output(Chunk.buffer(out)) >> go(tl, buffer)
          case Some(_) =>
            Pull.raiseError(new Exception("Invalid internal state."))
        }

    go(s, new Buffer).stream
  }

}
