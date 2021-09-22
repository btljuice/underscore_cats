import cats.Eval
import scala.math.random


/* Eval is also a monad */

val now = Eval
  .now { println("Computing now"); random + 0 }
  .map { i => println("now.map"); i }

val later = Eval
  .later { println("Computing later"); random + 1 }
  .map { i => println("later.map"); i }

val always = Eval
  .always { println("Computing always"); random + 2 }
    .map { i => println("always.map"); i }

now.value
now.value

later.value
later.value

always.value
always.value

val sum = for {
  a <- Eval.now{ println("A"); 40 }
  b <- Eval.always{ println("B"); 20 }
} yield {
  println("A + B")
  a + b
}

sum.value
sum.value

val saying = Eval
  .always { println("A"); "The cat" }
  .map { s => println("B"); s"$s sat on" }
  .memoize
  .map { s => println("C"); s"$s the mat" }

saying.value
saying.value

def factorial(n: BigInt): Eval[BigInt] =
  if (n <= 2) Eval.now (n)
  else Eval.defer(factorial(n-1)).map(_ * n)

factorial(50000).value

def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): Eval[B] =
  as match {
    case Nil => Eval.now(acc)
    case head :: tail => Eval.defer(foldRight(tail, acc)(f)).map(f(head, _))
  }

foldRight(List.range(1, 1000000), 0)(_ + _).value
