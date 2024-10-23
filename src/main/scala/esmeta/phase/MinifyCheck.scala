package esmeta.phase

import scala.util.*
import scala.io.Source
import esmeta.CommandConfig
import esmeta.util.*
import esmeta.util.BaseUtils.*
import esmeta.parser.ESParser
import esmeta.spec.Spec
import esmeta.es.util.fuzzer.{MinifyChecker, MinifyCheckerConfig}
import esmeta.js.minifier.Minifier
import esmeta.util.SystemUtils.*

case object MinifyCheck extends Phase[Spec, Unit] {
  val name = "minify-check"
  val help = "check differences of ast between original and minified code."

  def apply(spec: Spec, cmdConfig: CommandConfig, config: Config): Unit =
    val fileName = getFirstFilename(cmdConfig, name)

    // Step 1: Read the content of a.js and b.js
    val codeA = Source.fromFile(fileName).getLines().mkString("\n")

    // Step 2: Initialize MinifyChecker
    val minifyFunction: String => Option[String] = code =>
      Minifier.minifySwc(code) match
        case Failure(exception) => println(s"[minify-check] $exception"); None
        case Success(minified)  => Some(minified)

    val checker = new MinifyChecker(spec, minifyFunction, MinifyCheckerConfig())

    // Step 3: Use the check method to compare the original code with its minified version
    val resultA = checker.check(codeA)

    // Step 4: Print the results
    println(s"Results for ${fileName}:")
    resultA match {
      case Some(res) =>
        println(s"Diff Number: ${res.diff.size}")
        println(s"Diff: ${res.diff.map(ast => (ast.name, ast)).mkString(", ")}")
        println(s"Original: ${res.original}")
        println(s"Minified: ${res.minified}")
      case None =>
        println("Minification failed.")
    }

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
