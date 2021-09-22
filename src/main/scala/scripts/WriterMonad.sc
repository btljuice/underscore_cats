
/* 4.7 Writer Monad */
import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._
import cats.syntax.writer._
import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration.DurationInt

Writer(
  Vector(
    "It was the best of times",
    "it was the worst of times"),
  1859)

type Logged[A] = Writer[Vector[String], A]

123.pure[Logged]
Vector("msg1", "msg2", "msg3").tell
val aWriter = 123.writer(Vector("msg1", "msg2", "msg3"))

val (log, result) = aWriter.run

val writer1 = for {
  a <- 10.pure[({type W[A] = Writer[Vector[String], A]})#W]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run

val writer2 = writer1.mapWritten(_.map(_.toUpperCase))
writer2.run

val writer3 = writer1.bimap(log => log.map(_.toUpperCase), res => res * 100)
writer3.run

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

def factorial(n: Int): Int = {
  val ans = slowly { if (n == 0) 1 else n * factorial(n -1) }
  println(s"fact $n $ans")
  ans
}

// Works fine for single-thread job
factorial(5)

// Logs are interleaved if jobs are run in parallel
Await.result(
  Future.sequence(Future(factorial(5)) :: Future(factorial(5)) :: Nil),
  5.seconds)
