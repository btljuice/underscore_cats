import cats.data.Reader

/**
 * cats.data.Reader is a monad that allows us to sequence operations that depend on some input.
 * Instances of Reader wrap up functions of one argument, providing us with useful methods for composing them.
 *
 * @note Reader is a specific usage Kleisli arrows
 */
final case class Cat(name: String, favoriteFood: String)
val lotus = Cat("Lotus", "Can food")

// Reader[A, B] === ReaderT[cats.ID, A, B]

val catName: Reader[Cat, String] = Reader(_.name)
val greetKitty: Reader[Cat, String] = catName.map(n => s"Hello $n")
val feedKitty: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

greetKitty.run(lotus)

val greetAndFeed = for {
  greet <- greetKitty
  feed <- feedKitty
} yield greet + '\n' + feed

greetAndFeed.run(lotus)


/* Exercise 4.8.3 */
final case class Db(
  usernames: Map[Int, String],
  passwords: Map[String, String],
)

type DbReader[A] = Reader[Db, A]

def findUserName(userId: Int): DbReader[Option[String]] =
  Reader { _.usernames.get(userId) }
def checkPassword(username: String, password: String): DbReader[Boolean] =
  Reader { _.passwords.get(username).contains(password) }
def checkLogin(userId: Int, password: String): DbReader[Boolean] =
  findUserName(userId).flatMap {
    case None => Reader[Db,Boolean](_ => false)
    case Some(username) => checkPassword(username, password)
  }

val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")

val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")

val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)
