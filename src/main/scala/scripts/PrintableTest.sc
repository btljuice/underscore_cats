import sandbox.Printable
import sandbox.Printable.Instances._
import sandbox.Printable.PrintableOps
import cats.Show
import cats.instances.int.catsStdShowForInt
import cats.instances.string.catsStdShowForString

final case class Cat(name: String, age: Int, color: String)
object Cat {
  implicit val printable: Printable[Cat] = cat => {
    // N.B. Can't use PrintableOps because String has a method format
    val name = Printable.format(cat.name)
    val age = Printable.format(cat.age)
    val color = Printable.format(cat.color)
    s"$name is $age year-old $color cat"
  }
  implicit val show: Show[Cat] = cat => {
    val name = Show.apply[String].show(cat.name)
    val age = Show.apply[Int].show(cat.age)
    val color = Show.apply[String].show(cat.color)
    s"<catz>$name is $age year-old $color cat</catz>"
  }
}

val cat = Cat("lotus", 2, "grey-white-brown")
cat.print()
Show.apply[Cat].show(cat)
