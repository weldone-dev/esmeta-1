package esmeta.peval.util

import esmeta.error.PartialEvaluatorError

object ESModels {

  /* name of ast which can call OrdinaryFunctionCreate */
  lazy val FUNC_DECL = "FunctionDeclaration"
  lazy val FUNC_EXPR = "FunctionExpression"
  lazy val ARROW_FUNC = "ArrowFunction"
  lazy val METHOD_DEF = "MethodDefinition"
  lazy val GEN_DECL = "GeneratorDeclaration"
  lazy val GEN_EXPR = "GeneratorExpression"
  lazy val ASYNC_GEN_DECL = "AsyncGeneratorDeclaration"
  lazy val ASYNC_GEN_EXPR = "AsyncGeneratorExpression"
  lazy val CLASS_FIELD_DEF = "FieldDefinition" // TODO check this
  lazy val CLASS_STATIC_BLOCK = "ClassStaticBlock" // TODO check this
  lazy val ASYNC_FUNC_DECL = "AsyncFunctionDeclaration"
  lazy val ASYNC_FUNC_EXPR = "AsyncFunctionExpression"
  lazy val ASYNC_ARROW_FUNC = "AsyncArrowFunction"

  enum FuncLikes {
    case FuncDecl, FuncExpr, ArrowFunc, MethodDef, GenDecl, GenExpr,
    AsyncGenDecl, AsyncGenExpr, ClassFieldDef, ClassStaticBlock, AsyncFuncDecl,
    AsyncFuncExpr, AsyncArrowFunc

    override def toString: String = this match
      case FuncDecl         => FUNC_DECL
      case FuncExpr         => FUNC_EXPR
      case ArrowFunc        => ARROW_FUNC
      case MethodDef        => METHOD_DEF
      case GenDecl          => GEN_DECL
      case GenExpr          => GEN_EXPR
      case AsyncGenDecl     => ASYNC_GEN_DECL
      case AsyncGenExpr     => ASYNC_GEN_EXPR
      case ClassFieldDef    => CLASS_FIELD_DEF
      case ClassStaticBlock => CLASS_STATIC_BLOCK
      case AsyncFuncDecl    => ASYNC_FUNC_DECL
      case AsyncFuncExpr    => ASYNC_FUNC_EXPR
      case AsyncArrowFunc   => ASYNC_ARROW_FUNC

    def paramAstName = this match
      case FuncDecl         => "FormalParameters" -> "FunctionBody"
      case FuncExpr         => "FormalParameters" -> "FunctionBody"
      case ArrowFunc        => ??? // "ArrowParameters" -> "ConciseBody"
      case MethodDef        => ??? // "UniqueFormalParameters" -> "FunctionBody"
      case GenDecl          => ???
      case GenExpr          => ???
      case AsyncGenDecl     => ???
      case AsyncGenExpr     => ???
      case ClassFieldDef    => ???
      case ClassStaticBlock => ???
      case AsyncFuncDecl    => ???
      case AsyncFuncExpr    => ???
      case AsyncArrowFunc   => ???

  }

  object FuncLikes {
    val valueSet = FuncLikes.values.map(_.toString).toSet

    def from(s: String): FuncLikes = s match
      case FUNC_DECL          => FuncDecl
      case FUNC_EXPR          => FuncExpr
      case ARROW_FUNC         => ArrowFunc
      case METHOD_DEF         => MethodDef
      case GEN_DECL           => GenDecl
      case GEN_EXPR           => GenExpr
      case ASYNC_GEN_DECL     => AsyncGenDecl
      case ASYNC_GEN_EXPR     => AsyncGenExpr
      case CLASS_FIELD_DEF    => ClassFieldDef
      case CLASS_STATIC_BLOCK => ClassStaticBlock
      case ASYNC_FUNC_DECL    => AsyncFuncDecl
      case ASYNC_FUNC_EXPR    => AsyncFuncExpr
      case ASYNC_ARROW_FUNC   => AsyncArrowFunc
      case _ => throw new PartialEvaluatorError(s"Unknown FuncLikes: ${s}")
  }

}

object ESFuncModeling {

  import esmeta.es.Ast
  import esmeta.state.{AstValue, Bool, Enum, Value}
  import esmeta.peval.{Known, Predict, Unknown}
  import ESModels.*

  lazy val FORMAL_PARAMS = "FormalParameters"
  lazy val ECMASCRIPT_CODE = "ECMAScriptCode"

  lazy val FUNC_BODY = "FunctionBody"

  case class ESFuncAst(
    params: Ast,
    funcBody: Ast,
    thisMode: Option[Enum],
    strict: Option[Boolean],
    decl: Ast,
    private val astType: FuncLikes,
  ) {
    val toRecordTname: String = "ECMAScriptFunctionObject"

    def toRecordEntries: List[(String, Predict[Value])] = List(
      FORMAL_PARAMS -> Known(AstValue(this.params)),
      ECMASCRIPT_CODE -> Known(AstValue(this.funcBody)),
      "ThisMode" -> (thisMode match
        case None       => Unknown
        case Some(mode) => Known(mode)
      ),
      "Strict" -> (strict match
        case None    => Unknown
        case Some(s) => Known(Bool(s))
      ),
    )
  }

  object ESFuncAst {
    def from(declAst: Ast): ESFuncAst = {
      val declType = FuncLikes.from(declAst.name)
      val params = AstHelper.getChildByName(declAst, FORMAL_PARAMS)
      val funcBody = AstHelper.getChildByName(declAst, FUNC_BODY)
      // ESMeta is always strict. if non-strict then ENUM_STRICT to ENUM_GLOBAL
      ESFuncAst(
        params = params,
        funcBody = funcBody,
        thisMode = {
          import FuncLikes.*
          import esmeta.state.{ENUM_STRICT, ENUM_LEXICAL}
          Some(declType match
            case FuncDecl         => ENUM_STRICT
            case FuncExpr         => ENUM_STRICT
            case ArrowFunc        => ENUM_LEXICAL
            case MethodDef        => ENUM_STRICT
            case GenDecl          => ENUM_STRICT
            case GenExpr          => ENUM_STRICT
            case AsyncGenDecl     => ENUM_STRICT
            case AsyncGenExpr     => ENUM_STRICT
            case ClassFieldDef    => ENUM_STRICT
            case ClassStaticBlock => ENUM_STRICT
            case AsyncFuncDecl    => ENUM_STRICT
            case AsyncFuncExpr    => ENUM_STRICT
            case AsyncArrowFunc   => ENUM_LEXICAL,
          )
        },
        strict = Some(true),
        astType = FuncLikes.from(declAst.name),
        decl = declAst,
      )

    }
  }

}

object ESHelper {
  export ESModels.*
  export ESFuncModeling.*
}
