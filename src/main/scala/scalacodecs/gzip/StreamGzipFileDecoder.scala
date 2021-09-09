package scalacodecs.gzip

import fs2.{Pipe, RaiseThrowable}
import scalacodecs.gzip.DeflateDecoder.{EndOfLastBlock, InternalSymbol}
import scodec.Decoder
import scodec.bits.BitVector
import scodec.stream.StreamDecoder

object StreamGzipFileDecoder {
  private val decoder: StreamDecoder[InternalSymbol] = {
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

  def toPipe[F[_]: RaiseThrowable]: Pipe[F, BitVector, InternalSymbol] =
    in => in.map(_.reverseBitOrder).through(decoder.toPipe)
}
