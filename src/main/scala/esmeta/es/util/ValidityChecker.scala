package esmeta.es.util

import scala.util.Try
import esmeta.*
import esmeta.es.*
import esmeta.spec.*
// TODO import esmeta.es.util.JSEngine
import esmeta.util.BaseUtils.warn

/** TODO ECMAScript program validity checker */
object ValidityChecker {
  // val MESSAGE = "VALIDITY_CHECKER_EXPECTED_EXCEPTION"

  // def apply(grammar: Grammar, ast: Ast): Boolean =
  //   apply(ast.toString(grammar = Some(grammar)))
  // def apply(code: String): Boolean =
  //   val src = s"${USE_STRICT}throw \"$MESSAGE\";$LINE_SEP;$LINE_SEP$code"
  //   if (JSEngine.useGraal) checkValid(JSEngine.runGraal(src, Some(1000)))
  //   else if (JSEngine.useD8) checkValid(JSEngine.runD8(src, Some(1000)))
  //   else if (JSEngine.useJs) checkValid(JSEngine.runJs(src, Some(1000)))
  //   else if (JSEngine.useNode) checkValid(JSEngine.runNode(src, Some(1000)))
  //   else
  //     warn("No JSEngine available. this may pass invalid program.")
  //     true

  // private def checkValid(result: Try[Any]): Boolean =
  //   result.failed.filter(_.getMessage contains MESSAGE).isSuccess
}
