import cats.data.Reader

final case class Cat(name: String, favoriteFood: String)
val lotus = Cat("Lotus", "Can food")

val catName: Reader[Cat, String] =
  Reader(_.name)
val greetKitty: Reader[Cat, String] =
  catName.map(n => s"Hello $n")
val feedKitty: Reader[Cat, String] =
  Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

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

type DbReader[T] = Reader[Db, T]

def findUserName(userId: Int): DbReader[Option[String]] =
  Reader { _.usernames.get(userId) }

def checkPassword(userName: String, password: String): DbReader[Boolean] =
  Reader { _.passwords.get(userName).exists(_ == password) }

def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
  userName <- findUserName(userId)
  samePassword <- userName
    .map { checkPassword(_, password) }
    .getOrElse[DbReader[Boolean]] { Reader(_ => false) }
} yield samePassword


val users = Map(1 -> "dade", 2 -> "kate", 3 -> "margo")

val passwords = Map("dade" -> "zerocool", "kate" -> "acidburn", "margo" -> "secret")

val db = Db(users, passwords)

checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)
