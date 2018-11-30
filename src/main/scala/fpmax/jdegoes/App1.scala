package fpmax.jdegoes

import scala.io.StdIn.readLine
import scala.util.Try

object App1 {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  case class IO[A] (unsafeRun: () => A) { self =>
    def map[B](f: A => B) : IO[B] = IO(() => f(self.unsafeRun))
    def flatMap[B](f: A => IO[B]): IO[B] = IO(() => f(self.unsafeRun).unsafeRun())
  }

  object IO {
    def point[A](a: =>A): IO[A] = IO(() => a)
  }

  def putStr(line: String): IO[Unit] = IO.point(print(line))
  def getStr: IO[String] = IO.point(readLine())

  def nextInt(upper: Int): IO[Int] = IO(() => scala.util.Random.nextInt(upper))

  def gameLoop(name: String): IO[Unit] =
    for {
      num <- nextInt(5).map(_ + 1)
      _ <- putStr("Dear " + name + ", please guess a number from 1 to 5:")
      input <- getStr
      _ <- parseInt(input).fold(putStr("_"))(guess =>
        if (guess == num) putStr("You guessed right, " + name + "!")
        else putStr("You guessed wrong, " + name + "! The number was: " + num))
      cont <- checkContinue(name)
      _ <- if(cont) gameLoop(name) else IO.point(())
    } yield ()

  def checkContinue(name: String): IO[Boolean] =
    for {
      _ <-putStr("Do you want to continue, " + name + "?")
      answer <- getStr.map(_.toLowerCase)
      b <- IO.point(answer match {
        case "y" => true
        case "n" => false
        case _ => true
      })
    } yield b

  def main(): IO[Unit] =
    for {
      _ <- putStr("What is your name?")
      name <- getStr
      _ <- putStr("Hello, " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
}
