package scalacodecs.gzip

import scodec.bits.ByteVector
import scodec.{Attempt, DecodeResult, Decoder}

import java.nio.file.{Files, Paths}

object Gunzip extends App {
  val bytes = ByteVector(Files.readAllBytes(Paths.get(args(0))))

  val dec = Decoder[GzipFileFormat]
  dec.decode(bytes.toBitVector) match {
    case Attempt.Successful(DecodeResult(gzip, _)) =>
      val p = Paths.get(gzip.fileName.getOrElse("out"))
      println(s"Decompressing to: ${p.toAbsolutePath}")
      Files.write(p, gzip.data.toArray)
    case Attempt.Failure(err) =>
      println(err)
  }
}
