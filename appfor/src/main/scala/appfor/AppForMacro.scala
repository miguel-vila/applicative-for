package appfor

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.api.Universe
import scala.collection.mutable.{ListBuffer, Stack}

trait Utils {
}

object AppForMacroImpl {

  type M = Validation[Int]

  def usesTerm(u: Universe)(term: u.ValDef, exp: u.Tree): Boolean = {
    val u.ValDef(_,termName,_,_) = term
    exp.find{
      case u.Ident(_termName) if termName == _termName =>
        true
      case _ =>
        false
    }.isDefined
  }

  def getFirstSegment(c: Context)(pairs: List[(c.universe.ValDef, c.Tree)]): (List[(c.universe.ValDef, c.Tree)], List[(c.universe.ValDef, c.Tree)]) = {
    def loop(pairs: List[(c.universe.ValDef, c.Tree)],
             terms: Set[c.universe.ValDef]
    ): (List[(c.universe.ValDef, c.Tree)], List[(c.universe.ValDef, c.Tree)]) = pairs match {
      case Nil        => (Nil, Nil)
      case (term,exp)::tail =>
        if( terms.forall( t => !usesTerm(c.universe)(t, exp) ) ) {
          val (tlSegment, rest) = loop(tail, terms + term)
          ((term,exp)::tlSegment, rest)
        } else {
          (Nil, (term,exp)::tail)
        }
    }
    loop(pairs, Set.empty)
  }

  def joinSegment(c: Context)(segment: List[(c.universe.ValDef, c.Tree)]): c.Tree = {
    import c.universe._
    segment match {
      case List((term, expr)) => expr
      case (term, expr)::rest =>
        val joinedRest = joinSegment(c)(rest)
        q"""_root_.appfor.Validation.applicativeInstance.map2()"""
        ???
    }
  }

  def getPairs(c: Context)(valid: c.Expr[M]): (List[(c.universe.ValDef, c.Tree)], c.Tree) = {
    import c.universe._
    val Apply(TypeApply(Select(firstMonadicValue, TermName(firstFunctionName)),_), List(continuation)) = valid.tree
    val Function(List(firstArgumentTerm), firstFunctionBody) = continuation
    if(firstFunctionName == "map") {
      (List((firstArgumentTerm, firstMonadicValue)), firstFunctionBody)
    } else if(firstFunctionName == "flatMap") {
      val (rest, finalExpr) = getPairs(c)(c.Expr(firstFunctionBody))
      ((firstArgumentTerm, firstMonadicValue) :: rest, finalExpr)
    } else {
      throw new Exception("!!!")
    }
  }

  def getPairsImpl(c: Context)(valid: c.Expr[M]): c.Expr[M] = {
    import c.universe._
    val (pairs, inner) = getPairs(c)(valid)
    println(s"pairs = $pairs")
    val (firstSegment, rest) = getFirstSegment(c)(pairs)
    println(s"firstSegment =$firstSegment")
    println(s"rest =$rest")
    valid
  }

  def app_for_impl(c: Context)(valid: c.Expr[M]): c.Expr[M] = {
    import c.universe._

    val Apply(TypeApply(Select(firstMonadicValue, TermName("flatMap")),_), List(functionDef)) = valid.tree
    val Function(List(firstArgumentTerm), functionBody) = functionDef
    val Apply(TypeApply(Select(secondMonadicValue, TermName("map")),_), List(secondFunctionDef)) = functionBody
    if(usesTerm(c.universe)(firstArgumentTerm, secondMonadicValue)) {
      valid
    } else {
      val Function(List(secondArgumentTerm), innerExpr) = secondFunctionDef
      c.Expr(q"""_root_.appfor.Validation.applicativeInstance.map2(
          $firstMonadicValue,
          $secondMonadicValue
      )(
          ${Function(List(firstArgumentTerm, secondArgumentTerm), innerExpr)}
      )""")
    }
  }

}

object AppForMacro {

  type M = Validation[Int]

  def app_for(valid: M): M =
    macro AppForMacroImpl.app_for_impl

  def pairs(valid: M): M =
    macro AppForMacroImpl.getPairsImpl

}
