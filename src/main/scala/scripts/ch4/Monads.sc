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

/*
 * - ~ A monad is a mechanism for sequencing computations ... w/ some complications ~
 *     You can redefine the F[_] part at each step of the sequencing
 * - ~ A functor is a mechanism for sequencing computations, but w/o complications
 *     You cannot redefine the F[_] part.
 */

// Simple example w/ Option
def parseInt(s: String): Option[Int] = Try(s.toInt).toOption
def divide(a: Int, b: Int) = Option.when(b != 0)(a / b)

// This is not possible w/ `.map` as `.map` requires a successful result value, while flatMap gives the ability to
// - Customize the creation of F[_] in intermediary steps of the process
// - Let the flatMap handle the default behaviors of the F[_] type ex:
//     - Option/Either/Future early outs on None/Error/Exception
//     - List computes all permutations are returns the flattened result
val divResultWFlatMap: Option[Int] =
  parseInt("8").flatMap { a =>
    parseInt("2").flatMap { b =>
      divide(a, b)
    }
  }

val divResultWMap: Option[Option[Option[Int]]] =
  parseInt("8").map { a =>
    parseInt("2").map { b =>
      divide(a, b)
    }
  }

type MyId[A] = A

/* 4.1.1. Monad definition.
 * Must obey
 * - Left Identity: pure(x).flatMap(f) == f(x)
 * - Right Identity: fa.flatMap(pure) == fa
 * - Associativity:
 *   fa.flatMap(f).flatMap(g) == fa.flatMap(a => f(a).flatMap(g))
 * */
trait MyMonad[F[_]] {
  // N.B. They are other minimal definitions, but we are sticking to this one.
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  /* 4.1.2 Every Monad is a Functor */
  final def map[A, B](fa: F[A])(f: A => B) = flatMap(fa)(x => pure(f(x)))
}

/** The purpose of the Id monad, is to be able to pass to generic monad function, plain values */
object MyMonad {
  implicit object idMonad extends MyMonad[MyId] {
    override def pure[A](a: A) = a
    override def flatMap[A, B](a: MyId[A])(f: A => MyId[B]) = f(a)
  }
}
implicit object idCatMonad extends cats.Monad[MyId] {
  override def pure[A](x: A) = x
  override def flatMap[A, B](a: MyId[A])(f: A => MyId[B]) = f(a)
  @tailrec override def tailRecM[A, B](a: A)(f: A => MyId[Either[A, B]]) =
    f(a) match {
      case Left(a) => tailRecM(a)(f)
      case Right(b) => b
    }
}

/** Usage: Create a generic function for any monad F */
def sumSquare[F[_]: Monad](a: F[Int], b:F[Int]): F[Int] = for {
  x <- a
  y <- b
} yield x*x + y*y

sumSquare(1 :: 2 :: 3 :: Nil, 1 :: 2 :: 3 :: Nil)

// This shows the value of the idMonad, where a generic monad function can be used against plain values
sumSquare[Id](1, 2)
sumSquare[MyId](1, 2)

/* 4.5.1 Monad errors */
// MonadError is like exposing a "second" pure/flatMap/map API for the error type
// Either String     | A
//        raiseError | pure
//        flatMap    | handleErrorWith
//        *map       | handleError (E => A)

type ErrorOr[+A] = Either[String, A]

val monadError: MonadError[ErrorOr, String] = MonadError[ErrorOr, String]

val success2 = monadError.pure(42)
val failure2 = monadError.raiseError("error")

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

// Show that it works for any MonadError
validateAdult[Try](18)
validateAdult[Try](8)
type ExceptionOr[A] = Either[Throwable, A]
validateAdult[ExceptionOr](8)
