package esmeta.peval

import esmeta.ir.{Func, Inst}
import esmeta.state.{State, Value}

import scala.collection.immutable.HashMap

class SpecializedFuncs(
  val map: Map[String, PartialFunction[(Iterable[Value], State), String]],
) {
  def getByArgs(
    funcName: String,
    args: Iterable[Value],
    st: State,
  ): Option[String] =
    map.get(funcName).flatMap(_.lift(args, st))
}

object SpecializedFuncs {
  val EMPTY = SpecializedFuncs(Map.empty)
}
