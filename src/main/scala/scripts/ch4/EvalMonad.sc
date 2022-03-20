import cats.Eval
import scala.math.random


/**
 * EvalMonad is a monad that allows us to abstract over different models of evaluation:
 *   - Eager: call-by-value
 *   - Lazy: call-by-name
 *   - Memoized: call-by-need
 *
 *
 * Eval is also stack-safe
 */


// scala `val` is eager (evaluated at definition); memoized (evaluated only once)
val x = {
  println("Computing x")
  math.random
}

//  scala `def` is lazy  (evaluated when called); not-memoized (evaluated at every use)
def y = {
  println("Computing y")
  math.random
}

//  scala `lazy val` is lazy (evaluated when called); memoized (evaluated at every use)
lazy val z = {
  println("Computing z")
  math.random
}

/*
* Note that not only EvalMonad abstract the different types of evaluation but it
* alsos provide map / flatMap
*/

/** Now => eager, memoized */
val now = Eval
  .now { println("Computing now"); random + 0 }
  .map { i => println("now.map"); i }

/** Later => lazy, memoized */
val later = Eval
  .later { println("Computing later"); random + 1 }
  .map { i => println("later.map"); i }

/** Always => lazy, non-memoized */
val always = Eval
  .always { println("Computing always"); random + 2 }
    .map { i => println("always.map"); i }

now.value
now.value

later.value
later.value

always.value
always.value

// Monad is useful within for comprehensions
val sum = for {
  a <- Eval.now{ println("A"); 40 }
  b <- Eval.always{ println("B"); 20 }
} yield {
  println("A + B")
  a + b
}

sum.value
sum.value

//  `.memoize` is quite useful to store the chain of computation!!!
val saying = Eval
  .always { println("A"); "The cat" }
  .map { s => println("B"); s"$s sat on" }
  .memoize
  .map { s => println("C"); s"$s the mat" }

saying.value
saying.value

// Defer delays execution af an Eval. It is also stack-safe
def factorial(n: BigInt): Eval[BigInt] =
  if (n <= 2) Eval.now (n)
  else Eval.defer(factorial(n-1)).map(_ * n)


val f150 = factorial(50000)

f150.value
f150.value

// Enable  writing stacksafe recursive methods A.K.A Trampolining
def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): Eval[B] =
  as match {
    case Nil => Eval.now(acc)
    case head :: tail => Eval.defer(foldRight(tail, acc)(f)).map(f(head, _))
  }

// This  would normally stack overflow under a  normal recursive implementation
foldRight(List.range(1, 1000000), 0)(_ + _).value
