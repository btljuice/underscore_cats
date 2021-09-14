import cats.Monad
import cats.MonadError
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.instances.list._
import cats.instances.either._
import cats.Id
import scala.annotation.tailrec

type MyId[A] = A
/* Must obey
 * - Left Identity: pure(x).flatMap(f) == f(x)
 * - Right Identity: fa.flatMap(pure) == fa
 * - Associativity:
 *   fa.flatMap(f).flatMap(g) == fa.flatMap(a => f(a).flatMap(g))
 * */
trait MyMonad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  final def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(x => pure(f(x)))
}
object MyMonad {
  implicit object idMonad extends MyMonad[MyId] {
    override def pure[A](a: A) = a
    override def flatMap[A, B](a: MyId[A])(f: A => MyId[B]) = f(a)
  }
}
implicit object idMonad2 extends Monad[MyId] {
  override def pure[A](x: A) = x
  override def flatMap[A, B](a: MyId[A])(f: A => MyId[B]) = f(a)
  @tailrec override def tailRecM[A, B](a: A)(f: A => MyId[Either[A, B]]) =
    f(a) match {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => b
    }
}

def sumSquare[F[_]: Monad](a: F[Int], b:F[Int]): F[Int] = for {
  x <- a
  y <- b
} yield x*x + y*y

sumSquare(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)

sumSquare[Id](1, 2)
sumSquare[MyId](1, 2)

/* 4.5.1 Monad errors */
type ErrorOr[+A] = Either[String, A]
val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

val success = monadError.pure(42)
val failure = monadError.raiseError("Bad state")
monadError.handleErrorWith(failure) {
  case "Bad state" => monadError.pure("It's ok")
  case _ => monadError.raiseError("It's not ok")
}

monadError.handleError(failure) {
  case "Bad state" => 42
  case _ => -1
}
monadError.ensure(success)("Number too low!")(_ > 1000)

