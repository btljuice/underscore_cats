import cats.syntax.either._
import scala.util.Try

val a = 3.asRight[String]
val b = 4.asRight[String]

for { x <- a; y <- b } yield x + y

Either.catchOnly[NumberFormatException]("foo".toInt)
Either.catchNonFatal(sys.error("Pouet"))
Either.fromTry(Try("foo".toInt))
Either.fromOption(None, "No value")


// From Scala
"Error".asLeft[Int] getOrElse 0
"Error".asLeft[Int] orElse 2.asRight[String]

// Cats
(-1).asRight[String].ensure("Must be non-negative")(_ >= 0)
"error".asLeft[Int].recover { case _:String => -1 }
"error".asLeft[Int].recoverWith { case _:String => Right(-1) }

"foo".asLeft[Int].leftMap(_.reverse)
6.asRight[String].bimap(_.reverse, _ * 7)
"foo".asLeft[Int].bimap(_.reverse, _ * 7)
"foo".asLeft[Int].swap

