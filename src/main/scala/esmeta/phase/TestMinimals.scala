package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.USE_STRICT
import esmeta.es.Script
import esmeta.interpreter.*
import scala.util.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.util.SystemUtils.*
import scala.collection.parallel.CollectionConverters._
import esmeta.es.util.fuzzer.*
import esmeta.es.util.fuzzer.MinifyTester
import esmeta.injector.*
import esmeta.state.State
import esmeta.util.BaseUtils.*
import esmeta.util.SystemUtils.*
import esmeta.js.JSEngine
import esmeta.js.minifier.Minifier
import esmeta.es.util.delta.DeltaDebugger
import scala.collection.mutable.{Map => MMap, Set => MSet}
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.mutator.TracerExprMutator

case object TestMinimals extends Phase[CFG, Unit] {
  val name = "test-minimals"
  val help = "test minimals"

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    println("Test minimals")

    val baseDir = getFirstFilename(cmdConfig, "test-minimals")

    var totalCount = 0
    var bugCount = 0

    val tracerExprMutator = TracerExprMutator(using cfg)
    val minifyTester = MinifyTester(
      cfg,
      MinifyTesterConfig(
        timeLimit = config.testTimeLimit,
        ignoreProperties = List("name").map(prop => s"\"$prop\""),
      ),
    )
    val tracerInjector = TracerInjector(using cfg)

    for {
      minimal <- listFiles(s"$baseDir/minimal").par
      name = minimal.getName
      if jsFilter(name)
      code = readFile(minimal.getPath).drop(USE_STRICT.length).strip
      script = Script(code, name)
    } {
      val state =
        Interpreter(
          cfg.init.from(script),
          log = false,
          detail = false,
          timeLimit = config.evalTimeLimit,
        )
      val injector = ReturnInjector(cfg, state, config.injectTimeLimit, false)
      val passed = injector.exitTag match
        case NormalTag =>
          val returns = injector.assertions
          val codes = code +: tracerExprMutator(code, 5, None).map(
            _._2.toString(grammar = Some(cfg.grammar)),
          )
          (for {
            ret <- returns.par
            code <- codes.par
          } yield {
            val instrumentedCode = tracerInjector(code)
            val iife = s"const k = (function () {\n$code\n$ret\n})();\n"
            val tracerHeader =
              s"const arr = []; const $TRACER_SYMBOL = x => (arr.push(x), x)\n"
            val original = USE_STRICT ++ tracerHeader ++ iife
            (minifyTester.test(original) match
              case None | Some(_: AssertionSuccess) => true
              case Some(failure)                    => false
            )
          }).fold(true)(_ && _)
        case _ => true
      if !passed then bugCount += 1
      totalCount += 1
    }

    println(s"Total: $totalCount, Bugs: $bugCount")
    println(s"Bug rate: ${bugCount.toDouble / totalCount * 100}%")

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
    (
      "eval-time-limit",
      NumOption((c, k) => c.evalTimeLimit = Some(k)),
      "set the evaluation time limit in seconds (default: no limit).",
    ),
    (
      "inject-time-limit",
      NumOption((c, k) => c.injectTimeLimit = Some(k)),
      "set the injection time limit in seconds (default: no limit).",
    ),
    (
      "test-time-limit",
      NumOption((c, k) => c.testTimeLimit = Some(k)),
      "set the test time limit in seconds (default: no limit).",
    ),
  )

  class Config(
    var out: Option[String] = None,
    var evalTimeLimit: Option[Int] = None,
    var injectTimeLimit: Option[Int] = None,
    var testTimeLimit: Option[Int] = None,
  )
}
