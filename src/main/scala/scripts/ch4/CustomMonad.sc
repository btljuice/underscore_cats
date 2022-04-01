import cats.Monad
import scala.annotation.tailrec
import cats.syntax.flatMap._ // For flatMap
import cats.syntax.functor._ // for map

// Any monads can be define by overriding the following methods
val optionMonad = new Monad[Option] {
  override def pure[A](x: A): Option[A] = Some(a)
  def none: Option[Nothing] = None

  override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

  @tailrec override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
    case Some(Left(a1)) => tailRecM(a1)(f)
    case Some(Right(b)) => Some(b)
    case None => None
  }

}

// tailRecM is an optimization because some flatMap are note stack-safe, if they are eagerly evaluated.

// A retry function is a good example. This will fail for Option
def retry[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
  f(start).flatMap { a =>
    retry(a)(f)
  }

// Tailrec version here is callstack safe since it's a @tailrec
def retryTailRecM[F[_] : Monad, A](start: A)(f: A => F[A]): F[A] =
  Monad[F].tailRecM(start) { a =>
    f(a).map(a2 => Left(a2))
  }

// because tailRecM is integrated in cats, there's some utils that leverages it:
import cats.syntax.monad._ // for iterateWhileM

def retryM[F[_] : Monad, A](start: A)(f: A => F[A]) = start.iterateWhileM(f)(_ => true)

/** Exercise 4.10.1 Monad for a Tree */
sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[A](value: A) extends Tree[A]

object treeMonad extends Monad[Tree] {
  override def pure[A](x: A) : Tree[A] = Leaf(a)
  override def flatMap[A, B](fa: Tree[A])(f: A => Tree[B]): Tree[B] = fa match {
    case Leaf(a) => f(a)
    case Branch(l, r) => Branch( flatMap(l)(f), flatMap(r)(f) )
  }

  @tailrec override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = f(a) match {
    case Leaf(Right(b)) => Leaf(b)
    case Leaf(Left(a)) => tailRecM(a)(f)
    case Branch(l, r) =>  ???
  }
}
