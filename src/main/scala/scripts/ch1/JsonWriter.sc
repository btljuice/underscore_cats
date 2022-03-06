import sandbox.ch1._
import sandbox.ch1.JsonWriter.Instances._
import sandbox.ch1.JsonWriter.Ops._

final case class Person(name: String, email: String)
val aPerson = Person("John Doe", "john.doe@nomail.com")

//// Ch 1.2. - JsonWriter

implicit val personWriter: JsonWriter[Person] = p => JsObject { Map(
  "name" -> JsString(p.name),
  "email" -> JsString(p.email),
) }

aPerson.toJson

// 1. What is the relationship of type classes instances related to a type and its subtype
//    Ex. Should JsonWrite[A] be defined as JsonWrite[A] or JsonWrite[+A] or JsonWrite[-A] ?
// 2. Which instance to select when there's many of them.
