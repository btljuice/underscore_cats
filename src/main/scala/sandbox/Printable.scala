package sandbox

trait Printable[A] {
  def format(a: A): String
}

object Printable {
  object Instances {
    def toStringPrintable[A]: Printable[A] = _.toString
    implicit val intPrintable: Printable[Int] = _.toString
    implicit val stringPrintable: Printable[String] = identity
  }

  implicit class PrintableOps[A](val a: A) extends AnyVal {
    def format(implicit p: Printable[A]): String = p.format(a)
    def print()(implicit p: Printable[A]): Unit = println(format)
  }
}