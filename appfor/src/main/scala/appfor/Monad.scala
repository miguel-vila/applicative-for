package appfor

trait Monad[F[_]] {
  def unit[A](a: A): F[A]
  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]
}
