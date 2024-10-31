package esmeta.peval.util

import esmeta.error.{PEvalOptError}
import scala.util.{Try, Success, Failure}

enum PEvalPolicy:
  case Never, Compute, ComputeAndUse
  def isNever = this match
    case Never => true
    case _     => false

  def shouldCompute = this match
    case Compute       => true
    case ComputeAndUse => true
    case _             => false

  def shouldUse = this match
    case ComputeAndUse => true
    case _             => false

  def toParsedString: String = PEvalPolicy.stringMap(this)

object PEvalPolicy:

  val stringMap = Map(
    Never -> "never",
    Compute -> "compute",
    ComputeAndUse -> "computeAndUse",
  )

  val parseMap = stringMap.map(_.swap)

  def parse(s: String): PEvalPolicy = parseMap.getOrElse(
    s,
    throw PEvalOptError(s"Invalid PEvalPolicy: $s"),
  )

  def parseOpt(s: String): Option[PEvalPolicy] = Try { parse(s) } match
    case Success(v) => Some(v)
    case Failure(_) => None
