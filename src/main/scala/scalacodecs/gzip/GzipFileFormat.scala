package scalacodecs.gzip

import scodec.Decoder
import scodec.bits.{ByteVector, HexStringSyntax}
import scodec.codecs._

case class GzipFileFormat(
  fileName: Option[String],
  data: ByteVector
)

object GzipFileFormat {
  implicit val decoder: Decoder[GzipFileFormat] =
    for {
      _     <- constant(hex"1f8b")
      _     <- ignore(8)
      flags <- uint8
      _     <- ignore(4 * 8)
      _     <- ignore(8)
      _     <- ignore(8)
      _     <- conditional((flags & (1 << 2)) != 0, variableSizeBytes(uint8, byte))
      name  <- conditional((flags & (1 << 3)) != 0, cstring)
      _     <- conditional((flags & (1 << 4)) != 0, cstring)
      _     <- conditional((flags & (1 << 2)) != 0, byte(2))
      data  <- new DeflateDecoder
      _     <- ignore(4 * 8)
      _     <- ignore(4 * 8)
    } yield GzipFileFormat(name, data)
}
