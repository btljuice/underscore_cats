import cats.Monad
import cats.MonadError
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.syntax.monadError._
import cats.instances.list._
import cats.instances.try_._
import cats.instances.either._
import cats.Id
import scala.annotation.tailrec
import scala.util.Try

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

val success = 42.pure[ErrorOr]
val failure = "Bad state".raiseError[ErrorOr, Int]
failure.handleErrorWith {
  case "Bad state" => 42.pure[ErrorOr]
  case _ => "It's not ok".raiseError
}

failure.handleError {
  case "Bad state" => 42
  case _ => -1
}
success.ensure("Number too low!")(_ > 1000)


val exn: Throwable = new RuntimeException("some runtime exception")
exn.raiseError[Try, Int]

/* Exercise 4.5.4 */
def validateAdult[F[_]](age: Int)(implicit me: MonadError[F, Throwable]): F[Int] =
  age.pure[F].ensure(new IllegalArgumentException(s"$age < 18: must be an adult"))(_ >= 18)

validateAdult[Try](18)
validateAdult[Try](8)
type ExceptionOr[A] = Either[Throwable, A]
validateAdult[ExceptionOr](8)
