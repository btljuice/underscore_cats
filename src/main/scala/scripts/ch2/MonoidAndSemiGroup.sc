
//// Ch2.0 What's a SemiGroup and What's Monoid?

// Examples:
//   * NonEmptyList concatenation
//   * Strictly Positive Integer sum
// Law:
//   * Associativity
//     combine(a, combine(b, c)) == combine(combine(a, b), c)
trait MySemiGroup[A] {
  def combine(l: A, r: A): A
}

// Examples:
//    * List / String concatenation
//    * addition, multiplication
// Bad example:
//    * substraction => not associative
// Law:
//    * Associativity
//      combine(a, combine(b, c)) == combine(combine(a, b), c)
//    * Identity
//      combine(a, unit) == combine(unit, a) == a
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
    val multMonoid: MyMonoid[Int] = MyMonoid(1, _ * _)
    val stringConcatMonoid: MyMonoid[String] = MyMonoid("", _ + _)
    val booleanOrMonoid: MyMonoid[Boolean] = MyMonoid(false, _ || _)
    val booleanAndMonoid: MyMonoid[Boolean] = MyMonoid(true, _ && _)

    def setUnionMonoid[A]: MyMonoid[Set[A]] = MyMonoid(Set.empty, _ | _)
    def setIntersectMonoid[A]: MyMonoid[Set[A]] = MyMonoid(Set.empty, _ &  _)
    def symDiffMonoid[A]: MyMonoid[Set[A]] = MyMonoid(Set.empty, (a, b) => (a | b) diff (a & b))
  }

  object Laws {
    def testAll[A](values: List[A])(implicit m: MyMonoid[A]): Boolean =
      testIdentity(values) && testAssociativity(values)

    def testIdentity[A](values: List[A])(implicit m: MyMonoid[A]): Boolean =
      values.forall { v => m.combine(m.unit, v) == v && m.combine(v, m.unit) == v }

    def testAssociativity[A](values: List[A])(implicit m: MyMonoid[A]): Boolean = (
        for { a <- values; b <- values; c <- values } yield testAssociativity(a, b, c)
    ).forall(identity)

    def testAssociativity[A](a: A, b: A, c: A)(implicit m: MyMonoid[A]): Boolean =
      m.combine(a, m.combine(b, c)) == m.combine(m.combine(a, b), c)
  }
}

val values = true :: false :: Nil
MyMonoid.Laws.testAll(values)(MyMonoid.Instances.booleanOrMonoid)
MyMonoid.Laws.testAll(values)(MyMonoid.Instances.booleanAndMonoid)

//// Cats Monoid and SemiGroup

import cats.kernel.Monoid
import cats.kernel.Semigroup
import cats.kernel.instances.int._
import cats.kernel.instances.option.catsKernelStdMonoidForOption
import cats.kernel.instances.string.catsKernelStdMonoidForString
import cats.kernel.instances.map._
import cats.syntax.monoid._

// Expeditive import
// import cats._
// import cats.implicits_

Monoid.combine("Hi ", "There")
Monoid[String].empty
Semigroup.combine("It takes", "two")
"Hi" |+| " There"
1 |+| 2


//// Exercise 2.5.4
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
add(Some(Order(1.0, 2.0)) :: None :: Some(Order(3.0, 4.0)) :: Nil)

/* 3.6.2. Given a Monoid[A] and a Bijection between A <=> B, then one can create a Monoid[B] */
def imap[A: Monoid, B](f: A => B)(g: B => A): Monoid[B] = new Monoid[B] {
  override def empty = f(Monoid[A].empty)
  override def combine(x: B, y: B) = f(Monoid[A].combine(g(x), g(y)))
}

implicit val symbolMonoid: Monoid[Symbol] = imap[String, Symbol](Symbol(_))(_.name)
'An |+| 'Other |+| 'Symbol

//// 2.7 Application and usages

/* Semi-Group and Monoid use cases
*   - Combine parallelized data
*   - Merge partial data to eventually have full information
*/

"Scala" |+| " with " |+| "Cats"

Option(1) |+| None |+| Some(2)

// Consolidate per keys and combine on values
Map('a' -> 1, 'b' -> 2) |+| Map('b' -> 3, 'c' -> 3)

// Generic add over lists
def addAll[A](xs: List[A])(implicit m: Monoid[A]): A = m.combineAll(xs) // equivalent to xs.foldLeft(m.empty)(m.combine)


