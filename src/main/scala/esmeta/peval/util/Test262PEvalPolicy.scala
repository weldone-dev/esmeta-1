package esmeta.peval.util

import esmeta.error.{PEvalOptError}
import scala.util.{Try, Success, Failure}

case class Test262PEvalPolicy(
  harness: PEvalPolicy,
  individual: PEvalPolicy,
  concurrentHarness: Boolean,
):

  def isNever = this.harness.isNever && this.individual.isNever

  def toParsedString: String = Test262PEvalPolicy.stringMap(this)

object Test262PEvalPolicy:

  val DEFAULT = Test262PEvalPolicy(PEvalPolicy.Never, PEvalPolicy.Never, false)

  lazy val values =
    for {
      p1 <- PEvalPolicy.values
      p2 <- PEvalPolicy.values
      b <- List(true, false)
    } yield Test262PEvalPolicy(p1, p2, b)

  val stringMap =
    values
      .map(v =>
        v -> s"${v.harness.toParsedString}-${v.individual.toParsedString}-${v.concurrentHarness}",
      )
      .toMap

  val parseMap = stringMap.map(_.swap)

  def parse(s: String): Test262PEvalPolicy = parseMap.getOrElse(
    s,
    throw PEvalOptError(s"Invalid Test262PEvalPolicy: $s"),
  )

  def parseOpt(s: String): Option[Test262PEvalPolicy] = Try { parse(s) } match
    case Success(v) => Some(v)
    case Failure(_) => None
