package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import scala.util.*
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.es.util.fuzzer.FSTrieWrapper

case object FSTrieStats extends Phase[Unit, Unit] {
  val name = "fstrie-stats"
  val help = "extract the statistics from a given fstrie json file."

  def apply(unit: Unit, cmdConfig: CommandConfig, config: Config): Unit =
    val baseDir = getFirstFilename(cmdConfig, "trie-statistics")
    val trie = FSTrieWrapper.fromDir(baseDir, fixed = true)
    val stacks = trie.stacks
    val stacksWithScores = trie.stacksWithScores
    val stats = stacksWithScores.toList.sortBy(_._2).groupBy(_._1.size)
    for (size <- stats.keys.toList.sorted) {
      val stacks = stats(size)
      println(f"Number of stacks with size $size: ${stacks.size}")
    }
    if config.out.isEmpty then println(stats.asJson)
    else dumpJson(stats, config.out.get)
    ()

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
  )

  class Config(
    var out: Option[String] = None,
  )
}
