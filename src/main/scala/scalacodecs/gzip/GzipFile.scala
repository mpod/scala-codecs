package scalacodecs.gzip

import scodec.bits.{ByteVector, HexStringSyntax}
import scodec.codecs._
import scodec.{Decoder, Err}

case class GzipFile(
  fileName: Option[String],
  data: ByteVector
)

object GzipFile {

  private val cstring2 =
    Decoder(bits =>
      cstring.decode(bits).mapErr {
        case Err.General(_, _) => Err.insufficientBits(bits.size + 1, bits.size)
        case err               => err
      }
    ).decodeOnly

  private[gzip] val header: Decoder[Option[String]] =
    for {
      _     <- constant(hex"1f8b")
      _     <- ignore(8)
      flags <- uint8
      _     <- ignore(4 * 8)
      _     <- ignore(8)
      _     <- ignore(8)
      _     <- conditional((flags & (1 << 2)) != 0, variableSizeBytes(uint8, byte))
      name  <- conditional((flags & (1 << 3)) != 0, cstring2)
      _     <- conditional((flags & (1 << 4)) != 0, cstring2)
      _     <- conditional((flags & (1 << 2)) != 0, byte(2))
    } yield name

  private[gzip] val tail: Decoder[Unit] =
    ignore(2 * 4 * 8)

  implicit val decoder: Decoder[GzipFile] =
    for {
      name <- header
      data <- new DeflateDecoder
      _    <- tail
    } yield GzipFile(name, data)
}
