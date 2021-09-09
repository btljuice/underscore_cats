import cats.Functor
import cats.instances.list._
import cats.instances.option._
import cats.instances.function._
import cats.syntax.functor._ // for map


trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  final def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  final def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
}

/* Applying a function over "whatever" is "inside" representation of the functor */
val list1 = List(1, 2, 3)
val list2 = Functor[List].map(list1)(_ * 2)
val option1 = Option(123)
val option2 = Functor[Option].map(option1)(_.toString)

/* Any function A => B can be lifted to a F[A] => F[B] via it's functor */
val func = (x: Int) => x + 1
val liftedFunc = Functor[Option].lift(func)

liftedFunc(Option(1))

/* `.as` to set invariably a value === list1.map(_ => "As") */
Functor[List].as(list1, "As")


/* method chaining through functors */
val func1 = (a: Int) => a + 1
val func2 = (a: Int) => a * 2
val func3 = (a: Int) => s"${a}!"
val func4 = func1.map(func2).map(func3)

func4(123)

/* Generalizing a method for all functors: */
def doMath[F[_] : Functor](fa: F[Int]): F[Int] = {
  Functor[F].map(fa) { x => (x + 1) * 2 }
}
doMath(1 :: 2 :: 3 :: Nil)
doMath(None: Option[Int])
doMath(Option(2))
//doMath(Map(1 -> 2, 3 -> 4))


/* cats provides extension method for functors */
import cats.syntax.functor._ // for map
final case class Box[+A](a: A)
object Box {
  implicit object functor extends Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B) = Box(f(fa.a))
  }
}
Box(1).map(_ + 1)

/* Exercise 3.5.4 */
sealed trait Tree[+A]
final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]
final case class Leaf[+A](value: A) extends Tree[A]
object Tree {
  implicit object functor extends Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B) = fa match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }
  }
}

val t: Tree[Int] = Branch(
  Leaf(1),
  Branch(
    Branch(Leaf(2), Leaf(3)),
    Branch(Leaf(4), Branch(Leaf(5), Leaf(6))),
  )
)
Tree.functor.map(t)(_ + 1)
// t.map(_ + 1) Does not work in worksheet

/* Functor's map is like "appending" a transformation to a chain */
