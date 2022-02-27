import cats.data.State
import scala.util.Failure
import scala.util.Success
import scala.util.Try

// f can:
// 1. Transform the state to a new one
// 2. Compute a result from the state
val a = State[Int, String]{ state => (state, s"The state is $state") }

a.run(10).value  // Gets both state and result
a.runS(10).value // Only care about the state
a.runA(10).value // Only care about the result

val add1 = State[Int, String]{ num =>
  val ans = num + 1
  (ans, s"Result of add1: $ans")
}
val times2 = State[Int, String]{ num =>
  val ans = num * 2
  (ans, s"Result of times2: $ans")
}

// map, flatMap can chain step together
val chained = for {
  r1 <- add1
  r2 <- times2
} yield (r1, r2)

chained.run(20).value

// copy the current state to the return value
val finalState = chained.get.runA(20).value

val demoChain = for {
  _ <- State.set(42)      // Invariably sets the status to 51 and return value to ()
  r1 <- State.get         // Get the current state
  r2 <- State.pure("foo") // leaves state intact and return 52
  r3 <- State.inspect[Int, String](i => s"bar with $i")
  _ <- State.modify[Int](i => i*i)
} yield s"r1 = $r1; r2 = $r2; r3 = $r3"

val (demoState, demoResult) = demoChain.run(1).value


/* 4.9.3 Exercise */
sealed trait Input
final case class Number(d: Double) extends Input
object Plus extends Input
object Minus extends Input
object Times extends Input
object Divide extends Input

type PoCalc[A] = State[PoCalc.Stack, A]
object PoCalc {
  type Result = Either[Error, Value]
  type Error = String
  type Value = Double
  type Stack = List[Value]

  def apply(f: Stack => (Stack, Result)): PoCalc[Result] = State[Stack, Result](f)

  val unit: PoCalc[Unit] = State.pure(())
  val zero: PoCalc[Result] = State.pure(Right(0.0))

  def reduce(f: (Value, Value) => Value, msg: => Error) = PoCalc {
    case x :: y :: tail =>
      val result = Try(f(x, y)).toEither.left.map(_.toString)
      (tail , result)
    case l => (l, Left(msg + ": Not enough number onto the stack"))
  }

  val plus = reduce(_ + _, "Cannot add numbers")
  val minus = reduce(_ - _, "Cannot subtract numbers")
  val times = reduce(_ * _, "Cannot multiply numbers")
  val divide = reduce(_ / _, "Cannot divide numbers")

  def push(calc: PoCalc[_])(input: Input): PoCalc[Result] = calc.get.flatMap { _ =>
    input match {
      case n: Number => PoCalc { stack => (n.d :: stack, Right(n.d)) }
      case Plus => plus
      case Minus => minus
      case Times => times
      case Divide => divide
    }
  }
}

val inputs = Number(1) :: Number(2) :: Plus :: Nil
inputs.foldLeft(PoCalc.zero){ case (calc, i) =>
  val result = PoCalc.push(calc)(i).map(v => println(s"Value = $v"))
  result
}


