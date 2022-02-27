package sandbox.ch1

sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case object JsNull extends Json


// writer typeclass
trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {
  def apply[A: JsonWriter]: JsonWriter[A] = implicitly[JsonWriter[A]]

  object Instances {
    implicit val stringWriter: JsonWriter[String] = JsString(_)
    implicit val numberWriter: JsonWriter[Double] = JsNumber(_)
    implicit def optionWriter[A: JsonWriter]: JsonWriter[Option[A]] = {
      case Some(value) => Json.toJson(value)
      case None => JsNull
    }
  }

  object Ops {
    implicit class JsonWriterExtension[A](val a: A) extends AnyVal {
      def toJson(implicit w: JsonWriter[A]): Json = Json.toJson(a)
    }
  }
}

object Json {
  def toJson[A: JsonWriter](a: A): Json = JsonWriter[A].write(a)
}
