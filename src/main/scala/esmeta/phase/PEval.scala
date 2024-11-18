package esmeta.phase

import esmeta.*
import esmeta.error.*
import esmeta.es.*
import esmeta.ir.{Func, Program}
import esmeta.peval.*
import esmeta.peval.util.*
import esmeta.spec.Spec
import esmeta.util.*
import esmeta.util.SystemUtils.*

/** `peval` phase */
case object PEval extends Phase[Program, Program] {
  val name = "peval"
  val help = "partial-evaluate a pre-defined target AO using an ES file."

  def apply(
    program: Program,
    cmdConfig: CommandConfig,
    config: Config,
  ): Program = {

    if (config.simplify < 0 || config.simplify > 3) then
      throw new PEvalOptError("config.simplify should be in [0, 3]")

    val target = program.funcs
      .find(_.name == ES_PE_TARGET)
      .getOrElse(
        throw PEvalOptError(
          s"peval target ${ES_PE_TARGET} not found in Program",
        ),
      )

    val filename = getFirstFilename(cmdConfig, name)
    val fds = {
      val ast = program.spec.scriptParser.fromFile(filename)
      AstHelper.getPEvalTargetAsts(ast)
    }

    val newProg = ESPartialEvaluator.pevalThenConstruct(
      program,
      fds.zipWithIndex.map((fd, idx) =>
        (fd, Some(s"${target.name}PEvaled${idx}")),
      ),
    )(
      log = None,
      detail = None,
      timeLimit = None,
      simplifyLevel = 1,
    )

    if (config.log) then
      val pw = getPrintWriter(s"$PEVAL_LOG_DIR/summary")
      pw.println(s"Found ${fds.length} function declarations in ${filename}");
      pw.flush
    // dumpTo(PEVAL_LOG_DIR, overloads.map(_._1));

    newProg
  }

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "turn on logging mode.",
    ),
    (
      "detail-log",
      BoolOption((c, b) => { c.log ||= b; c.detail = b }),
      "turn on logging mode with detailed information.",
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
    // var timeLimit: Option[Int] = None,
    // var multiple: Boolean = false,
    var log: Boolean = false,
    var detail: Boolean = false,
    var simplify: Int = 1,
  )

  /** dump CFG */
  def dumpTo(baseDir: String, funcs: List[Func]): Unit =
    val dirname = s"$baseDir/overloads"
    dumpDir(
      name = "p-evaled functions",
      iterable = ProgressBar("Dump p-evaled functions", funcs, detail = false),
      dirname = dirname,
      getName = func => s"${func.name}.ir",
    )
}
