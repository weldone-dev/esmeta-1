package esmeta.phase

import esmeta.*
import esmeta.cfgBuilder.CFGBuilder
import esmeta.interpreter.*
import esmeta.ir.Program
import esmeta.state.*
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.es.*

// TODO sort imports
import esmeta.peval.PartialEvaluator
import esmeta.peval.pstate.{PContext, PState}
import esmeta.peval.Unknown.get

/** `ir-peval` phase */
case object IRPEval extends Phase[Program, Program] {
  val name = "ir-peval"
  val help =
    "p-eval zero-arity funcs in Program. Skip by default if not turned on explicitly."

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = run(config, program)

  def run(config: Config, prog: Program): Program =
    if (!config.use) then prog
    else
      val logger = Some(getPrintWriter(s"$IRPEVAL_LOG_DIR/log"))
      Program(
        prog.funcs.flatMap {
          case f if !f.params.isEmpty => f :: Nil
          case f => {
            val (newF, forks) =
              PartialEvaluator.run(prog, f) { (_renamer, _pst) => }(
                logPW = logger,
                detailPW = None, // TODO add to Config
                simplifyLevel = config.simplify,
                timeLimit = None,
              )
            // `forks` are called by name, and newF keeps original name
            // since it has empty params, it will be called by its original name
            val fs = newF :: forks;
            if (config.log) then
              for (f <- fs) do {
                val pw = getPrintWriter(s"$IRPEVAL_LOG_DIR/${f.name}.ir")
                pw.println(newF.toString())
                pw.flush
                pw.close
              }
            fs
          }
        },
        prog.spec,
      )

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "timeout",
      NumOption((c, k) => c.timeLimit = Some(k)),
      "set the time limit in seconds (default: no limit).",
    ),
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "turn-on",
      BoolOption(_.use = _),
      "turn on peval mode. (if not given, ir-peval skipped by default.)",
    ),
    (
      "s",
      NumOption(_.simplify = _),
      """set level of simplify strategy.
    0: do nothing
    1 : flatten
    2 (default): flatten & syntactic optimization (reference transparency)
    3: flatten & syntactic & semantic optimization (use-def chain)""",
    ),
  )
  case class Config(
    var timeLimit: Option[Int] = None,
    var log: Boolean = false,
    var use: Boolean = false,
    var simplify: Int = 1,
  )
}
