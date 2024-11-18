package esmeta.peval.pstate

import esmeta.ir.Local

/** Element of PEvaluator's CallStack
  *
  * @param ctxt
  * @param retId
  *   used for re-printing. callee should not directly modify caller's context.
  */
case class PCallContext(ctxt: PContext /* ,  retId: Local */ )
// NOTE retId was used for printing return - now we don't use this
