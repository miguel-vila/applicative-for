package appfor

trait Applicative[F[_]] {
  def pure[A](a: A): F[A]
  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C]
}
