import sandbox.Printable
import sandbox.Printable.Instances._
import sandbox.Printable.PrintableOps
import cats.Show
import cats.syntax.show._
import cats.instances.int.catsStdShowForInt
import cats.instances.string.catsStdShowForString
import java.util.Date

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
    val name = cat.name.show
    val age = cat.age.show
    val color = cat.color.show
    s"<catz>$name is $age year-old $color cat</catz>"
  }
}

val cat = Cat("lotus", 2, "grey-white-brown")
cat.print()
Show.apply[Cat].show(cat)

implicit val dateShow: Show[Date] = d => s"${d.getTime}ms in since the Epoch"
