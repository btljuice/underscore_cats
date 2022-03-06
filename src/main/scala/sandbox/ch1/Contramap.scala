package sandbox.ch1

trait Contramap[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}
