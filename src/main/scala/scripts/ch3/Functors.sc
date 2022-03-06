import cats.Functor
import cats.implicits.toContravariantOps
import cats.implicits.toInvariantOps
import cats.instances.list._
import cats.instances.map._
import cats.instances.option._
import cats.instances.function._
import cats.syntax.functor._
import sandbox.ch1.Printable
import sandbox.ch1.Printable.Instances._
import scala.concurrent.ExecutionContext
import scala.concurrent.Future // for map


/*
 * A parametrized type F[A], can be transformed via an arbitrary "inner type" transformation A => B
 * Law:
 *   Identity: map(a)(id) == a
 *   Composition: map( map(a)(f) )(g) == map(a)(g . f)
 */
trait MyFunctor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  /* Any function A => B can be lifted to a F[A] => F[B] via it's functor */
  final def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)

  /* Any F[A] can be forcibly be converted to a F[B] through constant value */
  final def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
}

/* Function composition is a functor */

implicit val fn0CompositionFunctor: MyFunctor[Function0] = new MyFunctor[Function0] {
  override def map[A, B](fa: () => A)(f: A => B) = () => f(fa())
}

/** Uses the ?, which comes for the kind-projector plugin. Really neat! */
implicit def fn1CompositionFunctor[A]: MyFunctor[A => ?] = new MyFunctor[A => ?] {
  override def map[B, C](f: A => B)(g: B => C) = a => g(f(a))
}

implicit def fn2CompositionFunctor[A, B]: MyFunctor[(A, B) => ?] = new MyFunctor[(A, B) => ?] {
  override def map[C, D](f: (A, B) => C)(g: C => D) = (a, b) => g(f(a, b))
}



/* Usage 1: Applying a function over "whatever" is "inside" representation of the functor */
val list1 = Functor[List].map(List(1,2,3))(_ * 2)
val option1 = Functor[Option].map(Option(123))(_.toString)

/* Usage 2: Any function A => B can be lifted to a F[A] => F[B] via it's functor */
val func = (x: Int) => x.toString
val liftedFunc: Option[Int] => Option[String] = Functor[Option].lift(func)

liftedFunc(Option(1))

/* Usage 3: `.as` to set invariably a value === list1.map(_ => "As") */
Functor[List].as(list1, "SameValue")


/* Usage 4: method chaining through functors */
val func1 = (a: Int) => a + 1
val func2 = (a: Int) => a * 2
val func3 = (a: Int) => s"${a}!"
val func4 = func1.map(func2).map(func3)

func4(123)

/* Usage 5: Generalizing a method for all functors: */
def doMath[F[_] : Functor](fa: F[Int]): F[Int] = {
  Functor[F].map(fa) { x => (x + 1) * 2 }
}
doMath(1 :: 2 :: 3 :: Nil)
doMath(None: Option[Int])
doMath(Option(2))
doMath(Map(1 -> 2, 3 -> 4))

/* 3.5.3. Define Functor for custom types */
implicit val optionFunctor: Functor[Option] = new Functor[Option] {
  override def map[A, B](fa: Option[A])(f: A => B) = fa match {
    case Some(value) => Some(f(value))
    case None => None
  }
}

/* Define functor that has implicit dependencies: Use an implicit def */
implicit def futureFunctor(implicit ec: ExecutionContext) = new Functor[Future] {
  override def map[A, B](fa: Future[A])(f: A => B) = fa.map(f)
}

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
//t.map(_ + 1) // ANSME: Does not work in worksheet

/*
 * Functor's contramap is like "prepending" a transformation to a chain
 * - See example in Printable
 * - Usually you can provide a map if type A is in output
 * - Usually you can provide a contramap if the type A is in input
 * - Usually you provide a invariant map (imap) when A is both input and output
 * - The contramap method only makes sense for data types that represent transformations
 *   ANSME: Don't understand exactly what this comment means.
 */

// Define a generic Printable[Box[_]] through contramap
implicit def boxPrintable[A: Printable]: Printable[Box[A]] =
  implicitly[Printable[A]].contramap[Box[A]](_.a)


trait MyContravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}
trait MyInvariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

trait Codec[A] { self =>
  def encode(input: A): String
  def decode(s: String): A
}
object Codec {
  def apply[A: Codec]: Codec[A] = implicitly[Codec[A]]

  // The idea is that we'll "proxy" encoding of B via A.
  implicit object catsInvariant extends cats.Invariant[Codec] {
    override def imap[A, B](fa: Codec[A])(f: A => B)(g: B => A)  = new Codec[B] {
      override def encode(input: B) = fa.encode(g(input))
      override def decode(s: String) = f(fa.decode(s))
    }
  }

  implicit val stringCodec: Codec[String] = new Codec[String] {
    override def encode(input: String) = input
    override def decode(s: String) = s
  }
  implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble)(_.toString)
  implicit def boxCodec[A: Codec]: Codec[Box[A]] = Codec[A].imap(Box(_))(_.a)
}

