import diesel._, cats._, cats.implicits._
import simulacrum._

object DieselDemo extends App {

  @typeclass trait Combineable[A] {
    @op("|~|") def combinez(x: A, y: A): A
  }

  implicit val combinedInt: Combineable[Int] = new Combineable[Int] {
    def combinez(x: Int, y: Int) = x + y
  }

  @diesel
  trait Maths[F[_]] {
    def times(l: Int, r: Int): F[Int]
    def add(l: Int, r: Int): F[Int]
  }


  @diesel
  trait Logger[F[_]] {
    def info(s: String): F[Unit]
  }


  // Import companion-to-interpreter aliasing sugar
  import Maths.ops._, Logger.ops._
  import Combineable.ops._

  def prog[F[_]: Monad: Maths: Logger](x: Int, y: Int): F[Int] = {
    for {
      p <- Maths.times(x, y)
      _ <- Logger.info(s"Product: $p")
      s <- Maths.add(x, y)
      _ <- Logger.info(s"Sum: $s")
      f <- Maths.add(p, s)
      _ <- Logger.info(s"Final: $f")
      lol = p |~| s
      _ <- Logger.info(s"OMG: ${lol}")
    } yield f
  }

  // Wire our interpreters in
  implicit val mathsInterp = new Maths[Id] {
    def times(l: Int, r: Int) = l * r
    def add(l: Int, r: Int) = l + r
  }
  implicit val loggingInterp = new Logger[Id] {
    def info(msg: String) = println(msg)
  }

  val _ = prog[Id](3, 4)

}