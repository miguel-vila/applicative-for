package appfor

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.reflect.api.Universe
import scala.collection.mutable.{ListBuffer, Stack}

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

}
