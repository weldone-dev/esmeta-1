package esmeta.web

import esmeta.cfg.CFG

def debugger: Debugger = _debugger.get
def initDebugger(cfg: CFG, sourceText: String): Unit =
  _debugger = Some(Debugger(cfg.init.from(sourceText)))
private var _debugger: Option[Debugger] = None
