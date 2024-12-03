package esmeta.collector

import esmeta.util.BaseUtils.*
import esmeta.cfg.*
import esmeta.ir.{Func => IRFunc, *}
import esmeta.util.Loc
import esmeta.util.SystemUtils.getPrintWriter
import java.io.PrintWriter
import esmeta.{COLLECT_LOG_DIR}
import scala.collection.mutable.{Map => MMap}

class Collector(cfg: CFG, log: Boolean = false) {
  def run: Unit = {
    val specMap: MMap[(String, String), Int] = MMap()

    for { func <- cfg.funcs; node <- func.nodes } do
      val repr = node match
        case Block(_, insts, _) => insts.head.langOpt
        case node: NodeWithInst =>
          node.inst match
            case Some(inst) => inst.langOpt
            case None       => None
      for { lang <- repr; loc <- lang.loc } do
        val k = (func.irFunc.name, loc.stepString)
        val v = specMap.get(k) match
          case Some(prev) => prev min node.id
          case None       => node.id
        specMap += (k -> v)

    if (log)
      val pw: PrintWriter = getPrintWriter(s"$COLLECT_LOG_DIR/log")
      for {
        ((f, s), nid) <- specMap.toSeq.sortBy(_._1)
      } do pw.println(s"$f : step $s -> ${nid}")
      pw.flush()
      pw.close()
  }
}
