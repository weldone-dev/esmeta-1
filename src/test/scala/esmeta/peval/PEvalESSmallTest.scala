package esmeta.peval

import esmeta.*
import esmeta.ir.NormalInsts
import esmeta.peval.util.{AstHelper}
import esmeta.util.SystemUtils.*
import scala.util.{Try, Success, Failure}
import esmeta.cfgBuilder.CFGBuilder

/** eval test */
class PEvalESSmallTest extends PEvalTest {
  val name: String = "PEvalESSmallTest"

  // registration
  def init: Unit = for (file <- walkTree(ES_TEST_DIR)) {
    // test for function-declaration-instantiation
    val filename = file.getName
    if (jsFilter(filename)) check(filename) {
      val jsName = file.toString
      val irName = PEvalTest.js2ir(jsName)
      val insts = NormalInsts.fromFile(irName)

      val ast = PEvalTest.scriptParser.fromFile(jsName)
      val decls = AstHelper.getPEvalTargetAsts(ast)

      if !decls.isEmpty then {

        val (overloadByAst, overloadByName) = ESPartialEvaluator.peval(
          ESMetaTest.program,
          decls.zipWithIndex.map((fd, idx) => (fd, Some(s"__PEVALED__${idx}"))),
        )( /* run with default option */ );

        val sfMap =
          ESPartialEvaluator.genMap(overloadByAst)
        val newCFG = CFGBuilder
          .byIncremental(
            ESMetaTest.cfg,
            overloadByAst.map(_._1) ::: overloadByName,
            sfMap,
          )
          .getOrElse(fail("Cfg incremental build fail"))

        PEvalTest.checkExit(
          PEvalTest.evalFile(newCFG, jsName, checkAfter = insts),
        )
      }
    }
  }

  init
}
