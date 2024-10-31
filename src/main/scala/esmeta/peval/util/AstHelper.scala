package esmeta.peval.util

import esmeta.error.{InvalidAstField, PartialEvaluatorError}
import esmeta.es.{Ast, Lexical, Syntactic}
import esmeta.es.util.{UnitWalker as AstUnitWalker}
import esmeta.ir.{Expr}
import esmeta.peval.util.{ESHelper}
import esmeta.state.{Str}

// TODO : sort imports
import scala.collection.mutable.ListBuffer
import esmeta.peval.Unknown.get

object AstHelper {

  private class AstFinder(names: Set[String]) extends AstUnitWalker {
    val asts = ListBuffer.empty[Ast]
    def getAst: List[Ast] = asts.toList
    override def walk(lex: Lexical) =
      if names.contains(lex.name) then lex +=: asts
    override def walk(syn: Syntactic) =
      super.walk(syn)
      if names.contains(syn.name) then syn +=: asts
  }

  val getChildByName = (ast: Ast, name: String) =>
    getAllChildrenByName(ast, name).headOption.getOrElse(
      throw InvalidAstField(ast, Str(name)),
    )

  val getPEvalTargetAsts = (rootAst: Ast) =>
    import ESHelper.*
    // XXX : edit this to get all function declarations
    val asts = getAllChildrenByNames(
      rootAst,
      Set(ESHelper.FUNC_DECL, ESHelper.FUNC_EXPR),
    )
    asts.map((decl) => ESFuncAst.from(decl))

  def getAllChildrenByName(ast: Ast, name: String): List[Ast] =
    getAllChildrenByNames(ast, Set(name))

  def getAllChildrenByNames(ast: Ast, names: Set[String]): List[Ast] =
    val astFinder = new AstFinder(names)
    astFinder.walk(ast)
    astFinder.getAst

  /** auxilaray function for partial evaluators Sdo call
    *
    * @param ast
    *   a root ast to be explored
    * @param sub
    *   a sub ast to be found
    *
    * @return
    *   an expression representing the sub ast.
    * @throws InvalidAstField
    *   if the sub ast is not found.
    */
  def getSubgraphPath(root: Ast, sub: Ast): List[Int] =
    /* aux for getSubgraphPath to do recursion */
    def aux(now: Ast): Option[List[Int]] = now == sub match
      case true => Some(Nil)
      case false =>
        lazy val result = (for {
          (childOpt, idx) <- now.children.zipWithIndex
          child <- childOpt
          sublist <- aux(child)
          result = idx :: sublist
        } yield result).headOption
        result
    aux(root) match
      case Some(value) => value
      case None =>
        throw PartialEvaluatorError(s"getRepresent: $sub not found in $root")

}
