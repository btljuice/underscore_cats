import cats.Semigroup
// 1. What is the relationship of type classes instances related to a type and its subtype
//    Ex. Should JsonWrite[A] be defined as JsonWrite[A] or JsonWrite[+A] or JsonWrite[-A] ?
// 2. Which instance to select when there's many of them.

// Examples:
// * NonEmptyList concatenation
// * Strictly Positive Integer sum
// Law: Associativity
trait MySemiGroup[A] {
  def combine(l: A, r: A): A
}

// Examples:
// * List / String concatenation
// * addition, multiplication
// Bad example:
// * substraction => not associative
// Law: Associativity + Identity
trait MyMonoid[A] extends MySemiGroup[A] {
  def combine(l: A, r: A): A
  def unit: A
}
object MyMonoid {
  def apply[A](u: A, f: (A, A) => A) = new MyMonoid[A] {
    override def combine(l: A, r: A) = f(l, r)
    override def unit = u
  }

  object Instances {
    def listConcatMonoid: MyMonoid[List[_]] = MyMonoid(Nil, _ ::: _)
    val sumMonoid: MyMonoid[Int] = MyMonoid(0, _ + _)
    val stringConcatMonoid: MyMonoid[String] = MyMonoid("", _ + _)
    val booleanOrMonoid: MyMonoid[Boolean] = MyMonoid(false, _ || _)
    val booleanAndMonoid: MyMonoid[Boolean] = MyMonoid(true, _ && _)

    def setUnionMonoid[A]: MyMonoid[Set[A]] = MyMonoid(Set.empty, _ | _)
    def setIntersectMonoid[A]: MyMonoid[Set[A]] = MyMonoid(Set.empty, _ &  _)
  }
}

// Identity law
def testIdentity[A](values: List[A])(implicit m: MyMonoid[A]): Boolean = {
  val results =
    for { v <- values } yield {
      (m.combine(m.unit, v) == v) ::
      (m.combine(v, m.unit) == v) :: Nil
    }

  results.flatten.forall(identity)
}

def testAssociativity[A](values: List[A])(implicit m: MyMonoid[A]): Boolean = {
  val results =
    for { a <- values ; b <- values ; c <- values } yield {
      m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
    }

  results.forall(identity)
}

val values = true :: false :: Nil
testIdentity(values)(MyMonoid.Instances.booleanOrMonoid)
testIdentity(values)(MyMonoid.Instances.booleanAndMonoid)
testAssociativity(values)(MyMonoid.Instances.booleanOrMonoid)
testAssociativity(values)(MyMonoid.Instances.booleanAndMonoid)

// Cats Monoid and SemiGroup
import cats.Monoid
import cats.Semigroup
import cats.instances.string.catsKernelStdMonoidForString
import cats.instances.int._
import cats.syntax.monoid._
import cats.implicits.catsKernelStdMonoidForOption

Monoid.combine("Hi ", "There")
Monoid[String].empty
Semigroup.combine("It takes", "two")
"Hi" |+| " There"
1 |+| 2


// 2.5.4
def add[A: Monoid](items: List[A]): A = Monoid[A].combineAll(items)

add(List(1, 2, 3))
add(List(Some(1), None, Some(2), None, Some(3)))

case class Order(totalCost: Double, quantity: Double)
object Order {
  implicit val monoid: Monoid[Order] = Monoid.instance(
    Order(.0, .0),
    (l, r) => Order(l.totalCost + r.totalCost, l.quantity + r.quantity)
  )
}
add(Order(1.0, 2.0) :: Order(3.0, 4.0) :: Nil)
