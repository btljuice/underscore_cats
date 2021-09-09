package sandbox

trait Printable[A] { self =>
  def format(a: A): String
  def contramap[B](f: B => A): Printable[B] = b => format(f(b))
}

object Printable {
  def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

  def fromToString[A]: Printable[A] = _.toString

  implicit class PrintableOps[A](val a: A) extends AnyVal {
    def format(implicit p: Printable[A]): String = p.format(a)
    def print()(implicit p: Printable[A]): Unit = println(format)
  }


  object Instances {
    implicit val intPrintable: Printable[Int] = fromToString
    implicit val stringPrintable: Printable[String] = identity
  }
}