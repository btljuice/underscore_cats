
/* 4.7 Writer Monad */
import cats.data.Writer
import cats.instances.vector._

Writer(
  Vector(
    "It was the best of times",
    "it was the worst of times"),
  1859)
