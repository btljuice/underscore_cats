//// Ch 1.4. - cats.Show

import cats.Show
import cats.syntax.show._
import cats.instances.int.catsStdShowForInt
import cats.instances.string.catsStdShowForString
import java.util.Date

final case class Cat(name: String, age: Int, color: String)
val aCat = Cat("Lotus", 3, "grey-brown with white chest")

implicit val show: Show[Cat] = cat => {
  val name = cat.name.show
  val age = cat.age.show
  val color = cat.color.show
  s"<catz>$name is $age year-old $color cat</catz>"
}

Show[Cat].show(aCat)

aCat.show

implicit val dateShow: Show[Date] = d => s"${d.getTime}ms in since the Epoch"

new Date().show

