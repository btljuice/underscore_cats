import sandbox.ch1._
import sandbox.ch1.Printable.Instances._
import sandbox.ch1.Printable.Ops._

final case class Cat(name: String, age: Int, color: String)
final case class Person(name: String, email: String)
val aPerson = Person("John Doe", "john.doe@nomail.com")
val aCat = Cat("Lotus", 3, "grey-brown with white chest")

//// Ch 1.3. - Printable

implicit val personPrintable: Printable[Person] = p =>
  s"${p.name} e-mail is ${p.email}"
implicit val catPrintable: Printable[Cat] = c => {
  val name = Printable.format(c.name)
  val age = Printable.format(c.age)
  val color = Printable.format(c.color)
  s"$name is a $age year old $color cat"
}

aPerson.print()

aCat.print()


