package esmeta.phase

import esmeta.{error => _, *}
import esmeta.es.util.Coverage
import esmeta.cfg.CFG
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.es.util.USE_STRICT
import esmeta.es.util.fuzzer.MinifierDB
import esmeta.es.Script
import scala.util.*
import java.util.concurrent.atomic.AtomicLong
import esmeta.util.SystemUtils.*
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*

case object CoverageInvestigate extends Phase[CFG, Unit] {
  val name = "coverage-investigate"
  val help = "investigate newly covered parts by checking additional scripts."

  def apply(cfg: CFG, cmdConfig: CommandConfig, config: Config): Unit =
    import esmeta.ty.util.JsonProtocol.given
    given ConInvDataEncoder: Encoder[CovInvData] = deriveEncoder

    val covDir = getFirstFilename(cmdConfig, "coverage-investigate")

    val cov = Coverage.fromLogSimpl(covDir, cfg)
    println(
      s"Coverage restored from $covDir (${cov.kFs}-F${if cov.cp then "CP"
      else ""}S)",
    )

    val minimals = if config.useDB then
      val db = MinifierDB.fromResource
      for {
        (label, minimals) <- db.map
        minimal <- minimals
      } yield Script(minimal, label)
    else
      val newJsDir = getSecondFilename(cmdConfig, "coverage-investigate")
      for {
        jsFile <- listFiles(newJsDir)
        name = jsFile.getName
        if jsFilter(name)
        code = readFile(jsFile.getPath).drop(USE_STRICT.length).strip
      } yield Script(code, name)

    val res = (for {
      script <- minimals.toList
    } yield {
      try
        cov.runAndCheckWithBlocking(script) match
          case (
                finalSt,
                updated,
                covered,
                blockings,
                coveredNode,
                coveredBranch,
              ) =>
            Some(
              CovInvData(
                name,
                covered,
                blockings.map(_.code),
                coveredNode.map(_.toString),
                coveredBranch.map(_.toString),
              ),
            )
      catch
        case e: Throwable =>
          println(s"Error in $name : ${e.getMessage}")
          // e.printStackTrace()
          None
    }).flatten.sortBy(_._2)

    val count = res.count(_.covered)
    println(s"$count out of ${res.length} survived")

    res.foreach {
      case CovInvData(name, covered, blockings, coveredNode, coveredBranch) => {
        println(f"$name%30s: ${if (covered) "alive" else "dead"}")
      }
    }

    for (filename <- config.out)
      dumpJson(
        data = res,
        filename = filename,
      )
    ()

  val defaultConfig: Config = Config()

  val options: List[PhaseOption[Config]] = List(
    (
      "out",
      StrOption((c, s) => c.out = Some(s)),
      "output json file path.",
    ),
    (
      "use-db",
      BoolOption(c => c.useDB = true),
      "use resources/minifyfuzz-db minimals.",
    ),
  )

  class Config(
    var out: Option[String] = None,
    var useDB: Boolean = false,
  )
}

sealed case class CovInvData(
  name: String,
  covered: Boolean,
  blockings: Set[String],
  coveredNode: Set[String],
  coveredBranch: Set[String],
)
