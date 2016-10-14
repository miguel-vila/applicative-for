package test

import appfor.AppForMacro._
import appfor.{ Validation , Success, Failure }

object Test extends App {

  val v1: Validation[Int] = Failure(List("error1"))
  val v2: Validation[Int] = Failure(List("error2"))

  val resultWhenApplicative = app_for{
    for {
      x <- v1
      y <- v2
    } yield x + y
  }

  println(resultWhenApplicative)

  val resultWhenMonad = app_for {
    for {
      x <- v1
      y <- if(x>0) v2 else v1
    } yield x + y
  }

  println(resultWhenMonad)

  val v3: Validation[Int] = Success(3)

  pairs {
    for {
      x <- v1
      y <- v2
      z <- v3
    } yield x + y +z
  }

}
