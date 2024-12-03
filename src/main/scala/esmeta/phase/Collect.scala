package esmeta.phase

import esmeta.*
import esmeta.cfg.CFG
import esmeta.mutator.*
import esmeta.parser.ESParser
import esmeta.spec.Grammar
import esmeta.util.*
import esmeta.util.SystemUtils.*
import esmeta.collector.Collector

/** `collect` phase */
case object Collect extends Phase[CFG, Unit] {
  val name = "collect"
  val help = "collects information for ECMAScript visualizer."
  def apply(
    cfg: CFG,
    cmdConfig: CommandConfig,
    config: Config,
  ): Unit = Collector(cfg, config.log).run

  def defaultConfig: Config = Config()
  val options: List[PhaseOption[Config]] = List(
    (
      "log",
      BoolOption(_.log = _),
      "dump the collected information.",
    ),
  )
  case class Config(var log: Boolean = false)
}
