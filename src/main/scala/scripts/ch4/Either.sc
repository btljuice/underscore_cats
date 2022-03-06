import cats.implicits.toShow
import cats.syntax.either._
import scala.util.Try

// Useful Either constructors.
// These are notably useful for foldLeft or other typed inferenced curried functions, where knowing the full Either[A,B]
// type is useful to the compiler.
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


/* 4.4.4. Either usage: Fail-fast error handling */
val divResult = for {
  a <- 1.asRight[String]
  b <- 0.asRight[String]
  c <- if (b == 0) "DIV0".asLeft[Int] // Fails here
  else (a / b).asRight[String]
} yield {
  c * 100
}

sealed trait LoginError extends Product with Serializable
final case class UserNotFound(username: String) extends LoginError
final case class PasswordIncorrect(username: String) extends LoginError
case object UnexpectedError extends LoginError

case class User(username: String, password: String)
object User {
  implicit object userShow extends cats.Show[User] {
    override def show(t: User) = s"username = ${t.username} with password = ${t.password}"
  }
}

type LoginResult = Either[LoginError, User]

object LoginError {
  implicit object loginErrorShow extends cats.Show[LoginError] {
    override def show(t: LoginError) = t match {
      case UserNotFound(username) => s"User not found. username=$username"
      case PasswordIncorrect(username) => s"password incorrect. username=$username"
      case UnexpectedError => s"Unexpected error"
    }
  }

}

implicit def eitherShow[A: cats.Show, B: cats.Show]: cats.Show[Either[A, B]] = _.fold(_.show, _.show)
val result1: LoginResult = User("john", "Doh!").asRight[LoginError]
result1.show
val result2: LoginResult =  UnexpectedError.asLeft[User]
result2.show

