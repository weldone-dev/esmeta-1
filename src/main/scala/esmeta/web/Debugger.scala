package esmeta.web

import esmeta.cfg.*
import esmeta.es.Ast
import esmeta.ir.{Func => IRFunc, *}
import esmeta.interpreter.Interpreter
import esmeta.lang.Syntax
import esmeta.spec.{Algorithm, SyntaxDirectedOperationHead}
import esmeta.state.*
import esmeta.util.Loc
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/** debugger extension of IR interpreter */
class Debugger(st: State) extends Interpreter(st, log = true) {
  // ---------------------------------------------------------------------------
  // shortcuts
  // ---------------------------------------------------------------------------
  private inline def cfg = st.cfg
  private inline def cursor = st.context.cursor
  private inline def func = cursor.func
  private inline def irFunc = func.irFunc
  private inline def algoOpt: Option[Algorithm] = irFunc.algo

  // ---------------------------------------------------------------------------
  // overrides IR interpreter
  // ---------------------------------------------------------------------------
  // transition for node to more fine-grained execution within block node
  override def step: Boolean = (super.step, cursor) match
    case (_, _: ExitCursor) if !st.callStack.isEmpty =>
      saveBpCounts // save counter
      val res = step
      triggerBreaks // trigger breakpoints
      res
    case (res, _) => res
  override def eval(node: Node): Unit = {
    saveBpCounts // save counter
    (cursor, node) match
      case (cursor: NodeCursor, block @ Block(_, insts, next)) =>
        eval(insts(cursor.idx))
        cursor.idx += 1
        if (cursor.idx == insts.length) {
          cursor.idx -= 1
          st.context.moveNode
        }
      case _ => super.eval(node)
    triggerBreaks // trigger breakpoints
  }

  // ---------------------------------------------------------------------------
  // execution control
  // ---------------------------------------------------------------------------

  /** step result */
  enum StepResult:
    case Breaked, Terminated, Succeed

  // step until given predicate
  // TODO handle yet
  @tailrec
  final def stepUntil(pred: => Boolean): StepResult =
    if (!isBreaked) {
      val keep = step
      if (pred && keep) stepUntil(pred)
      else if (keep) StepResult.Succeed
      else StepResult.Terminated
    } else StepResult.Breaked

  private def getIrInfo = ((irFunc.name, cursor.stepsOpt), st.callStack.size)

  // spec step
  final def specStep = {
    val (prevLoc, _) = getIrInfo
    stepUntil { prevLoc._2.isDefined && prevLoc == getIrInfo._1 }
  }

  // spec step over
  final def specStepOver =
    val (prevLoc, prevStackSize) = getIrInfo
    stepUntil {
      val (loc, stackSize) = getIrInfo
      (prevLoc._2.isDefined && prevLoc == loc) || (prevStackSize < stackSize)
    }

  // spec step out
  final def specStepOut =
    val (_, prevStackSize) = getIrInfo
    stepUntil { prevStackSize <= getIrInfo._2 }

  // es steps from ast span info
  private def getEsInfo =
    val ctxts = st.context :: st.callStack.map(_.context)
    val callStackSize = ctxts.count(_.isEsCall)
    val (ls, le) = ctxts.flatMap(_.esLocOpt).headOption.getOrElse((-1, -1))
    ((ls, le), callStackSize)

  // es step
  final def esStep =
    val (prevLoc, _) = getEsInfo
    stepUntil {
      val (loc, _) = getEsInfo
      (loc._1 == -1 || loc._1 != loc._2 || loc._1 == prevLoc._1)
    }

  // es step over
  final def esStepOver =
    val (prevLoc, prevStackSize) = getEsInfo
    stepUntil {
      val (loc, stackSize) = getEsInfo
      (loc._1 == -1 || loc._1 != loc._2 || loc._1 == prevLoc._1 || prevStackSize < stackSize)
    }

  // es step out
  final def esStepOut =
    val (_, prevStackSize) = getEsInfo
    stepUntil {
      val (loc, stackSize) = getEsInfo
      loc._1 == -1 || prevStackSize <= stackSize
    }

  // continue
  final def continue: StepResult = stepUntil { true }

  // ---------------------------------------------------------------------------
  // breakpoints
  // ---------------------------------------------------------------------------

  /** breakpoints used in debugger */
  trait Breakpoint {
    var enabled: Boolean
    private var prevCount = 0
    private var trigger = false
    def needTrigger: Boolean = {
      if (trigger) { trigger = false; true }
      else false
    }
    protected def on: Unit = trigger = true
    protected def count: Int
    def saveCount: Unit = prevCount = count
    def check: Unit = if (enabled && prevCount < count) this.on
    def toggle() = { enabled = !enabled }
    protected def getContexts: List[Context] =
      st.context :: st.callStack.map(_.context)
  }

  /** breakpoints by spec steps */
  case class SpecBreakpoint(
    fid: Int,
    steps: List[Int],
    var enabled: Boolean = true,
  ) extends Breakpoint {
    override protected def count: Int =
      getContexts.count(ctxt =>
        ctxt.func.id == fid && ctxt.cursor.stepsOpt == Some(steps),
      )
  }

  /** breakpoints by ECMAScript code line */
  case class EsBreakpoint(
    line: Int,
    var enabled: Boolean = true,
  ) extends Breakpoint {
    @tailrec
    private def isParent(ast: Ast, target: Ast): Boolean = ast.parent match
      case Some(parent) =>
        if (parent eq target) true
        else isParent(parent, target)
      case None => false

    // count the number of breakpoint target ast
    override protected def count: Int =
      val astList = getContexts.flatMap { ctxt =>
        ctxt.astOpt match
          case Some(ast) if ast.loc.isDefined =>
            val loc = ast.loc.get
            val (ls, le) = (loc.start.line, loc.end.line)
            if (ls != -1 && ls == le && ls == line) Some(ast)
            else None
          case _ => None
      }
      astList.filter(ast => astList.exists(isParent(_, ast))).length
  }

  // breakpoints
  private val breakpoints: ListBuffer[Breakpoint] = ListBuffer()

  // trigger breakpoints
  private def triggerBreaks: Unit =
    for (b <- breakpoints) b.check

  // save counter for each breakpoint
  private def saveBpCounts: Unit =
    for (b <- breakpoints) b.saveCount

  // add breakpoints from serialized data
  final def addBreak(
    data: (Boolean, Int, List[Int], Boolean),
  ): Unit =
    val (isEsBreak, num, steps, enabled) = data
    val bp =
      if (isEsBreak) EsBreakpoint(num, enabled)
      else SpecBreakpoint(num, steps, enabled)
    breakpoints += bp

  // remove breakpoint
  final def rmBreakAll: Unit = breakpoints.clear
  final def rmBreak(idx: Int): Unit = breakpoints.remove(idx.toInt)

  // toggle breakpoints
  final def toggleBreakAll: Unit = for { bp <- breakpoints } bp.toggle()
  final def toggleBreak(idx: Int): Unit = breakpoints(idx).toggle()

  // check if current step is in break
  private def isBreaked: Boolean = breakpoints.foldLeft(false) {
    case (acc, bp) => bp.needTrigger || acc
  }

  // ---------------------------------------------------------------------------
  // debugger info
  // ---------------------------------------------------------------------------

  /** extension for cursor */
  extension (cursor: Cursor) {

    /** get ir instruction of current cursor */
    def instOpt: Option[Inst] = cursor match
      case NodeCursor(_, node, idx) =>
        node match
          case Block(_, insts, _) => Some(insts(idx))
          case node: NodeWithInst => node.inst
      case _: ExitCursor => None

    /** get syntax object of current cursor */
    def langOpt = cursor.instOpt.fold(None)(_.langOpt)

    /** get location in spec of current cursor */
    def stepsOpt: Option[List[Int]] = cursor.langOpt match
      case Some(lang) if lang.loc.isDefined => Some(lang.loc.get.steps)
      case _                                => None
  }

  /** extension for context */
  extension (ctxt: Context) {

    /** ast of current context */
    def astOpt: Option[Ast] =
      if (ctxt.func.isRuntimeSDO) Some(ctxt.locals(NAME_THIS).asAst)
      else None

    /** check if es call */
    def isEsCall: Boolean = ctxt.name == "Call" || ctxt.name == "Construct"

    /** location of ECMAScript code */
    def esLocOpt: Option[(Int, Int)] = astOpt match
      case Some(ast) =>
        Some(ast.loc match
          case Some(loc) => (loc.start.line, loc.end.line)
          case None      => (-1, -1),
        )
      case None => None
  }

  /** extension for func */
  extension (func: Func) {

    /** check whether it is runtime SDO */
    def isRuntimeSDO: Boolean = func.irFunc.head match
      case Some(SyntaxDirectedOperationHead(_, _, false, _, _)) => true
      case _                                                    => false
  }

  /** heap information */
  def heapInfo = st.heap.map.map {
    case (addr, obj) => (addr.toString, obj.toString)
  }

  /** call stack information */
  def callStackInfo =
    def getInfo(c: Context) = (
      c.func.id,
      c.name,
      c.cursor.stepsOpt.getOrElse(List()),
      c.locals.collect {
        case (Name(name), v) => (name, v.toString)
      }.toList,
    )
    (st.context :: st.callStack.map(_.context)).map(getInfo(_))

  /** context information */
  def ctxtInfo(cid: Int) =
    def paramInfo(p: Param) = (
      p.lhs.name,
      p.optional,
      p.ty.toString,
    )

    val ctxts = (st.context :: st.callStack.map(_.context)).drop(cid)
    val (os, oe) = ctxts.flatMap(_.astOpt) match
      case curr :: _ if curr.loc.isDefined =>
        val loc = curr.loc.get
        (loc.start.offset, loc.end.offset)
      case _ => (-1, -1)
    val ctxt = ctxts.head
    val (nid, isExit) = ctxt.cursor match
      case NodeCursor(_, n, _) => (n.id, false)
      case _: ExitCursor       => (-1, true)
    val func = ctxt.func
    val irFunc = func.irFunc
    val code = irFunc.algo.map(_.code).getOrElse("")
    (
      func.id,
      irFunc.kind.ordinal,
      irFunc.name,
      irFunc.params.map(paramInfo(_)),
      func.toDot(nid, isExit),
      code,
      (os, oe),
    )
}
