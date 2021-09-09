package scalacodecs

import scalacodecs.gzip.GzipFile
import scodec.bits.ByteVector
import scodec.{Attempt, DecodeResult, Decoder}
import scopt.OParser

import java.io.File
import java.nio.file.{Files, Paths}

object ScalaCodecs extends App {

  case class Config(file: File = new File("."))

  val argsParserBuilder = OParser.builder[Config]

  val parser = {
    import argsParserBuilder._
    OParser.sequence(
      programName("scala-codecs"),
      head("scala-codecs"),
      help("help").text("prints this usage text"),
      arg[File]("<file>")
        .action((x, c) => c.copy(file = x))
        .text(".gz file")
    )
  }

  OParser.parse(parser, args, Config()) match {
    case Some(Config(file)) =>
      val bytes = ByteVector(Files.readAllBytes(file.toPath))
      val dec   = Decoder[GzipFile]
      dec.decode(bytes.toBitVector) match {
        case Attempt.Successful(DecodeResult(gzip, _)) =>
          val p = Paths.get(gzip.fileName.getOrElse("out"))
          Console.out.println(s"Decompressing ${file.getPath} to: ${p.toAbsolutePath}")
          Files.write(p, gzip.data.toArray)
        case Attempt.Failure(err) =>
          Console.err.println(err)
      }
    case _ =>
  }

}
