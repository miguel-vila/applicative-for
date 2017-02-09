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

  def joinSegment(c: Context)(pairs: Vector[(c.universe.ValDef, c.Tree)], innerExpr: c.Tree): c.Tree = {
    import c.universe._

    val segment = pairs.map {
      case  (ValDef(_,termName,_,_), tree) => (termName, tree)
    }.tails.toStream.filter(_.length > 0).reverse.tail

    val joined = segment.init.foldLeft(segment.head(0)._2) { case (acc, subsegment) =>
      println(subsegment)
      val (term,expr) = subsegment.head
      val termsTail = subsegment.tail.map(_._1)
      val termsPattern = termsTail map { termName =>
        Bind(termName, Ident(termNames.WILDCARD))
      }
      q"""_root_.appfor.Validation.applicativeInstance.map2($expr, $acc) {
        case ($term, (..$termsPattern)) =>
            ($term, ..$termsTail)
      }"""
    }

    val subsegment = segment.last
    val (term,expr) = subsegment.head
    val terms = subsegment.map(_._1)
    val termsPattern = terms.tail map { termName =>
      Bind(termName, Ident(termNames.WILDCARD))
    }
    println(s"FINAL TERMS PATTERN = $termsPattern")
    println(s"---------- $term")
    val finalExp = q"""_root_.appfor.Validation.applicativeInstance.map2($expr, $joined) {
        case (${Bind(term, Ident(termNames.WILDCARD))}, (..$termsPattern)) =>
            $innerExpr
      }"""
    println(s"FINAL: $finalExp")
    println(s"INNER: ${showRaw(innerExpr)}")
    finalExp

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

  def getPairsImpl(c: Context)(valid: c.Expr[M]): c.Tree = {
    import c.universe._
    val (pairs, inner) = getPairs(c)(valid)
    val joined = joinSegment(c)(pairs.toVector, inner)
    println(s" joined = $joined")
    joined
    /*
     println(s"pairs = $pairs")
    println(s"inner = $inner")
    val (firstSegment, rest) = getFirstSegment(c)(pairs)
    println(s"firstSegment =$firstSegment")
    println(s"rest =$rest")
    */
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

  def pairs(valid: M): Any =
    macro AppForMacroImpl.getPairsImpl

}
