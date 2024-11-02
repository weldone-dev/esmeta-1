package esmeta.peval.util

import esmeta.ir.*

object IRSimplifier {
  val flat = InstFlattener.apply
  val syntactic = RemoveUnusedDef.apply
  val semantic = UseDef.apply
}

object InstFlattener {

  def apply(inst: Inst): Inst = inst match
    case IIf(cond, thenInst, elseInst) =>
      IIf(cond, apply(thenInst), apply(elseInst)).passCmt(from = inst)
    case IWhile(cond, body) => IWhile(cond, apply(body)).passCmt(from = inst)
    case ISeq(insts) =>
      flat(ISeq(insts.map(apply))).insts match
        case head :: Nil => head
        case rest        => ISeq(rest)
    case INop() => ISeq(Nil)
    case i      => i

  private def flat(iseq: ISeq): ISeq = ISeq(iseq.insts.map {
    case ISeq(insts) => insts.map(apply)
    case i           => List(apply(i))
  }.flatten)
}

object UseDef {

  // WIP
  def apply(inst: Inst): Inst = inst
}
