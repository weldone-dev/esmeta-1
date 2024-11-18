package esmeta.peval

import esmeta.error.{NonLiteral, PartialEvaluatorError}
import esmeta.ir.*
import esmeta.state.*
import esmeta.util.{Flat, Zero, One, Many}
import scala.util.{Try}

type PValue = Predict[Value]
type PRefTarget = Predict[RefTarget]

extension (pv: PValue) {
  def isKnownLiteral: Boolean = pv match
    case Known(v) => v.isLiteralValue
    case _        => false

  def toFlat: Flat[Value] = pv match
    case Known(v) => One(v)
    case _        => Many
}

extension [T](ft: Flat[T]) {
  def getOrElse(default: => T): T = ft match
    case Zero      => default
    case One(elem) => elem
    case Many      => default

  def toPredict(zero: => Predict[T]): Predict[T] = ft match
    case Zero      => zero
    case One(elem) => Known(elem)
    case Many      => Unknown
}

extension (rf: RefTarget) {
  inline def known: PRefTarget = Known(rf)
}

extension (v: Value) {

  inline def known: PValue = Known(v)

  def toExpr: Expr = v match
    case Math(decimal)  => EMath(decimal)
    case Infinity(pos)  => EInfinity(pos)
    case Enum(name)     => EEnum(name)
    case CodeUnit(c)    => ECodeUnit(c)
    case Number(double) => ENumber(double)
    case BigInt(bigInt) => EBigInt(bigInt)
    case Str(str)       => EStr(str)
    case Bool(bool)     => EBool(bool)
    case Undef          => EUndef()
    case Null           => ENull()
    case _              => throw new NonLiteral(v)

  def toExprOpt: Option[Expr] = Try(v.toExpr).toOption

  def isLiteralValue: Boolean = v match
    case Math(_) | Infinity(_) | Enum(_) | CodeUnit(_) | Number(_) | BigInt(_) |
        Str(_) | Bool(_) | Undef | Null =>
      true
    case _ => false
}

extension (sc: StringContext) {
  inline def throwPeval(args: Any*): Nothing = throw new PartialEvaluatorError(
    sc.s(args),
  )
}

lazy val ES_PE_TARGET = ESPartialEvaluator.TARGET
