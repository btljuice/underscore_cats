package sandbox.ch1

trait Printable[A] {
  def format(a: A): String
}
object Printable {
  def apply[A: Printable]: Printable[A] = implicitly[Printable[A]]
  def fromToString[A]: Printable[A] = _.toString

  def format[A: Printable](a: A): String = apply[A].format(a)
  def print[A: Printable](a: A): Unit = println(format(a))

  object Instances {
    implicit val stringPrintable: Printable[String] = identity
    implicit val intPrintable: Printable[Int] = _.toString
  }

  object Ops {
    implicit class PrintableExtension[A](val a: A) extends AnyVal {
      def format(implicit p: Printable[A]): String = Printable.format(a)
      def print()(implicit p: Printable[A]): Unit = Printable.print(a)
    }
  }
}
