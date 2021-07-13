package scalazip

import scodec.bits.ByteVector
import scodec.{Attempt, DecodeResult, Decoder}

import java.nio.file.{Files, Paths}

object Gunzip extends App {
  val bytes = ByteVector(Files.readAllBytes(Paths.get(args(0))))

  val dec = Decoder[GzipFileFormat]
  dec.decode(bytes.toBitVector) match {
    case Attempt.Successful(DecodeResult(gzip, _)) =>
      Files.write(Paths.get(gzip.fileName.getOrElse("out")), gzip.data.toArray)
    case Attempt.Failure(err) =>
      println(err)
  }
}
