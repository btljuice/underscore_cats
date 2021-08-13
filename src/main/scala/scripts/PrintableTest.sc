import sandbox.Printable
import sandbox.Printable.Instances._
import sandbox.Printable.PrintableOps

final case class Cat(name: String, age: Int, color: String)
object Cat {
  implicit val printable: Printable[Cat] = a => {
    // N.B. Can't use PrintableOps because String has a method format
    val name = stringPrintable.format(a.name)
    val age = a.age.format
    val color = stringPrintable.format(a.color)
    s"$name is $age year-old $color cat"
  }
}

Cat("lotus", 2, "grey-white-brown").print()
