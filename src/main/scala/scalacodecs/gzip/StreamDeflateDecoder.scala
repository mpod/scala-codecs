package scalacodecs.gzip

import scodec.codecs._
import scodec.stream.StreamDecoder
import scodec.{Codec, Err}

object StreamDeflateDecoder {
  import DeflateDecoder._

  private def stopOnEndOfBlock(s: InternalSymbol): Codec[InternalSymbol] =
    s match {
      case EndOfBlock => fail[InternalSymbol](Err("Stop"))
      case _          => provide(s)
    }

  val decoder: StreamDecoder[InternalSymbol] =
    for {
      header <- StreamDecoder.tryMany(blockHeader)
      blockDecoder <-
        if (header.blockType == 0)
          StreamDecoder.once(nonCompressedDecoderArgs).map(nonCompressedDecoder)
        else if (header.blockType == 1)
          StreamDecoder.emit(fixedHuffmanDecoder)
        else if (header.blockType == 2)
          StreamDecoder.once(dynamicHuffmanDecoderArgs).map(dynamicHuffmanDecoder)
        else
          StreamDecoder.raiseError(Err.General("Invalid block type.", Nil))
      symbol <- {
        val symbols =
          if (header.blockType == 0)
            StreamDecoder.once(blockDecoder)
          else
            StreamDecoder.tryMany(blockDecoder.flatMap(stopOnEndOfBlock))
        val ignoreEndOfBlock =
          if (header.blockType == 0)
            StreamDecoder.empty
          else
            StreamDecoder.once(blockDecoder).flatMap(_ => StreamDecoder.empty)
        val endOfLastBlock =
          if (header.lastBlock)
            StreamDecoder.emit(EndOfLastBlock)
          else
            StreamDecoder.empty
        symbols ++ ignoreEndOfBlock ++ endOfLastBlock
      }
    } yield symbol
}
