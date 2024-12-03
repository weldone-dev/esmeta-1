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
    val specMap: MMap[(String, String), Node] = MMap()

    for { func <- cfg.funcs; node <- func.nodes } do
      val repr = node match
        case Block(_, insts, _) => insts.head.langOpt
        case node: NodeWithInst =>
          node.inst match
            case Some(inst) => inst.langOpt
            case None       => None
      for { lang <- repr; loc <- lang.loc } do
        val key = (func.irFunc.name, loc.stepString)
        specMap.get(key) match
          case Some(origin) =>
            if (origin.id > node.id) specMap += (key -> node)
          case None => specMap += (key -> node)

    if (log)
      val pw: PrintWriter = getPrintWriter(s"$COLLECT_LOG_DIR/log")
      for { ((f, s), n) <- specMap.toSeq.sortBy(_._1) } do
        pw.println(s"$f : step $s -> ${n.name}")
      pw.flush()
      pw.close()

    for { ((f, s), n) <- specMap.toSeq.sortBy(_._1) } do // !debug
      println(s"$f : step $s -> ${n.name}")
  }
}
