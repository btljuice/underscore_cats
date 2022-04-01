import cats.data.State
import scala.annotation.tailrec
import scala.collection.immutable.AbstractSeq
import scala.collection.immutable.LinearSeq
import scala.util.Failure
import scala.util.Success
import scala.util.Try

/**
 * cats.data.State allows us to pass additional state around as part of a computation.
 * We define State instances representing atomic state operations and thread them together using map and flatMap.
 * In this way we can model mutable state in a purely functional way, without using actual mutation
 */

// type State[S, A]  = StateT[Eval, S, A]
// Reader is a special case of State, where the state is always the last computed result

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

/* 4.9.3. Exercise */

sealed trait Symbol
final case class Number(x: Double) extends Symbol
sealed trait Operand extends Symbol
case object Plus extends Operand
case object Minus extends Operand
case object Times extends Operand
case object Divide extends Operand

object Symbol {
  def apply(s: String): Option[Symbol] =
    number(s) orElse operand(s)

  def number(s: String): Option[Number] = s.toDoubleOption.map(Number)
  def operand(s: String): Option[Operand] = s match {
    case "+" => Some(Plus)
    case "-" => Some(Minus)
    case "*" => Some(Times)
    case "/" => Some(Divide)
    case _ => None
  }
}

type PoCalc[A] = State[List[Double], A]

def push(x: Double): PoCalc[Double] = State { xs => (x :: xs, x) }
def operate(f: (Double, Double) => Double): PoCalc[Double] = State {
  case a :: b :: xs =>
    val c = f(a, b)
    (c :: xs,  c)
  case _ => sys.error("Unexpected, list must have at least 2 numbers")
}

def evalOne(s: Symbol): PoCalc[Double] = s match {
  case Number(x) => push(x)
  case Plus => operate(_ + _)
  case Minus => operate(_ - _)
  case Times => operate(_ * _)
  case Divide => operate(_ / _)
}

def toSymbols(s: String): List[Symbol] = s.trim.split(' ').map { Symbol(_).get }.toList

def calculate(ss: List[Symbol]): PoCalc[Double] = ss.foldLeft {
  State.pure[List[Double], Double](0.0)
} { (acc, s) => acc.flatMap(_ => evalOne(s)) }
.inspect { _.head }

calculate(toSymbols("1 2 +")).runA(Nil).value


