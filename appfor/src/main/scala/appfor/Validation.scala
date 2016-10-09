package appfor

sealed trait Validation[+A] {
  def flatMap[B](f: A => Validation[B]): Validation[B] = this match {
    case Success(value) => f(value)
    case Failure(error) => Failure(error)
  }
  def map[B](f: A => B): Validation[B] = flatMap(a => Success(f(a)))

}
case class Success[A](value: A) extends Validation[A]
case class Failure(errors: List[String]) extends Validation[Nothing]

object Validation {

  implicit object monadInstance extends Monad[Validation] {
    def unit[A](a: A) = Success(a)
    def flatMap[A,B](va: Validation[A])(f: A => Validation[B]): Validation[B] =
      va.flatMap(f)
  }

  implicit object applicativeInstance extends Applicative[Validation] {
    def pure[A](a: A) = Success(a)
    def map2[A,B,C](va: Validation[A], vb: Validation[B])(f: (A,B) => C): Validation[C] = {
      (va, vb) match {
        case (Success(a) , Success(b) ) => Success(f(a,b))
        case (Failure(ea), Failure(eb)) => Failure(ea ++ eb)
        case (Failure(ea), _          ) => Failure(ea)
        case (_          , Failure(eb)) => Failure(eb)
      }
    }
  }

}
