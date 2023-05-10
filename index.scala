//> using platform "js"
//> using scala "3.2.2"
//> using jsVersion "1.13.1"
//> using dep "co.fs2::fs2-io::3.7.0-RC5"
//> using jsModuleKind "common"

import cats.data.*
import cats.effect.*
import cats.effect.std.*
import cats.syntax.all.*
import fs2.*
import fs2.io.file.*
import fs2.io.process.*

object index extends IOApp.Simple:
  def getInput(input: String) =
    Env[IO].get(s"INPUT_${input.toUpperCase}")

  def getPath = getInput("path").map(_.foldMap(Path(_)))

  def getMigrations(path: Path) =
    Files[IO].list(path).compile.toList

  def getAdditions(path: Path) =
    Stream
      .resource(
        ProcessBuilder(
          "git",
          "diff-tree",
          "--no-commit-id",
          "--name-status",
          "-m",
          "-r",
          "HEAD",
          path.toString
        ).spawn[IO]
      )
      .flatMap(_.stdout)
      .through(fs2.text.utf8.decode)
      .through(fs2.text.lines)
      .filterNot(_.isEmpty)
      .map(_.split("\\s+").toList)
      .evalMap {
        case List(status, path) => IO.pure(Path(path))
        case l => IO.raiseError(new RuntimeException(s"Invalid diff: $l"))
      }
      .compile
      .toList

  def run = for
    path <- getPath
    migrations <- getMigrations(path)
    additions <- getAdditions(path)
    _ <- IO.raiseWhen(!migrations.endsWith(additions)) {
      new RuntimeException(
        s"Migration versions are not strictly increasing:\n${additions.mkString("\n")}"
      )
    }
  yield ()
