package sandbox.ch1

import cats.Contravariant
import cats.implicits.toContravariantOps

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
    /** Contramap usage example */
    implicit val boolPrintable: Printable[Boolean] = stringPrintable.contramap { case true => "yes"; case false => "no" }

    implicit object printableContraMap extends Contravariant[Printable] {
      override def contramap[A, B](fa: Printable[A])(f: B => A): Printable[B] = b => fa.format(f(b))
    }
  }

  object Ops {
    implicit class PrintableExtension[A](val a: A) extends AnyVal {
      def format(implicit p: Printable[A]): String = Printable.format(a)
      def print()(implicit p: Printable[A]): Unit = Printable.print(a)
    }
  }
}
