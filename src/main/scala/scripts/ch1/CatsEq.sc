import cats.Eq
import cats.syntax.eq._
import cats.syntax.option._
import cats.instances.int._
import cats.instances.long._
import cats.instances.option._
import java.util.Date

// One of cats.Eq goal is "type-safe" equality

/* == operator in scala is not type-safe */
1 == Some(1) // Compiles but gives false
1 != None // Compiles also

// Eq however is type-safe
// Eq[Int].eqv(1, Some(1)) // Does not compile
Eq[Int].eqv(1, 1)


/* Cats has === and =!= operators that are type safe */
// 1 === Some(1) // Does not compile
// 1 =!= Some(1) // Does not compile
1 === 1
1 =!= 2
// None =!= Some(1) Does not compile
Option(1) =!= Option.empty
1.some === Some(1)
1.some =!= none[Int]


implicit val dateEq: Eq[Date] = (d1, d2) => d1.getTime === d2.getTime


final case class Cat(name: String, age: Int, color: String)
object Cat {
  /* use == operator on each field */
  implicit val equality: Eq[Cat] = Eq.fromUniversalEquals
}

val cat1 = Cat("Garfield", 38, "orange and black")
val cat2 = Cat("Heathcliff", 33, "orange and black")

val optionCat1 = Option(cat1)
val optionCat2 = Option.empty[Cat]

// Compile error: cat1 === optionCat1
cat1 === cat2
optionCat1 =!= optionCat2
