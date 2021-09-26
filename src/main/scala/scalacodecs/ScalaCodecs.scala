package scalacodecs

import cats.effect.{ExitCode, IO, IOApp}
import fs2.Stream
import fs2.io.file.Files
import scalacodecs.gzip.StreamGzipFileDecoder
import scopt.OParser

import java.io.File
import java.nio.file.Path

object ScalaCodecs extends IOApp {

  case class Config(file: File = new File("."), streaming: Boolean = false)

  private val argsParserBuilder = OParser.builder[Config]

  private val parser = {
    import argsParserBuilder._
    OParser.sequence(
      programName("scala-codecs"),
      head("scala-codecs"),
      help("help").text("prints this usage text"),
      opt[Unit]("streaming")
        .action((_, c) => c.copy(streaming = true))
        .text("run in streaming mode"),
      arg[File]("<file>")
        .action((x, c) => c.copy(file = x))
        .text(".gz file")
    )
  }

  override def run(args: scala.List[String]): IO[ExitCode] = {
    val stream = for {
      config <- Stream.eval(IO.pure(OParser.parse(parser, args, Config()))).collect { case Some(c) => c }
      inputPath  = config.file.toPath
      outputPath = Path.of("").toAbsolutePath.resolve("out")
      _ <- Files[IO]
        .readAll(inputPath, 1)
        .through(StreamGzipFileDecoder.toPipeByte)
        .flatMap(b => Stream.emits(b.toByteArray))
        .through(Files[IO].writeAll(outputPath))
    } yield ()
    stream.compile.drain.as(ExitCode.Success)
  }

}
