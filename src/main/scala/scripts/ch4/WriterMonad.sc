
/* 4.7 Writer Monad */
import cats.data.Writer
import cats.data.WriterT
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.DurationInt


/**
 * cats.data.Writer is a monad that lets us carry a log/payload along with a computation.
 * We can use it to record messages, errors, or additional data about a computation,
 * and extract the log alongside the final result.
 */

// WriterT[F, L, V]
// type Writer[Id, L, V]

Writer( // WriterF[Id, Vector, Int]
  Vector(
    "It was the best of times",
    "it was the worst of times"),
  1859
)

type Logged[A] = Writer[Vector[String], A] // WriterT[Id, Vector[String, A]


123.pure[Logged]
Vector("msg1", "msg2", "msg3").tell.map(_ => 123)
val aWriter = 123.writer(Vector("msg1", "msg2", "msg3"))

val (log, result) = aWriter.run

// Logs are appended
val writer1 = for {
  a <- 10.pure[({type W[A] = Writer[Vector[String], A]})#W] // Needed to inline
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b // 42, abcxyz

writer1.run

val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
writer2.run // 42, ABCXYZ

val writer3 = writer1.bimap(log => log.map(_.toUpperCase), res => res * 100)
writer3.run // 4200, ABCXYZ

val writer4 = writer1.mapBoth { (log, res) =>
  val log2 = log.map(_ + '!')
  val res2 = res * 1000
  (log2, res2)
}
writer4.run

val writer5 = writer1.reset
writer5.run

val writer6 = writer1.swap
writer6.run

/* 4.7.3 Exercise factorial w/ logger: Use the WriterMonad instead */
def slowly[A](body: => A): A = try body finally Thread.sleep(100)

def factorial(n: Int): Logged[Int] = for {
  ans <- if (n <= 1) n.pure[Logged] else factorial(n - 1).map(n * _)
  _ <- Vector(s"fact $n $ans").tell
} yield ans

// Works fine for single-thread job
factorial(5).run

// Logs are interleaved if jobs are run in parallel
Await.result(
  Future.sequence(Future(factorial(5)) :: Future(factorial(5)) :: Nil),
  5.seconds)


type FutureLogged[A] = WriterT[Option, Vector[String], A]

for {
  () <- 5.pure[FutureLogged]
} yield ()
