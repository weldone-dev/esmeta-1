package esmeta.peval

import esmeta.{LINE_SEP, PEVAL_LOG_DIR, TEST_MODE}
import esmeta.analyzer.*
import esmeta.cfg.{Func as CFGFunc}
import esmeta.error.*
import esmeta.error.NotSupported.{*, given}
import esmeta.error.NotSupported.Category.{Type => _, *}
import esmeta.interpreter.*
import esmeta.ir.*
import esmeta.es.*
import esmeta.parser.{ESParser, ESValueParser}
import esmeta.peval.pstate.*
import esmeta.peval.simplifier.*
import esmeta.peval.util.*
import esmeta.state.{BigInt, *}
import esmeta.spec.{Spec}
import esmeta.util.BaseUtils.{error => _, *}
import esmeta.util.SystemUtils.*
import java.io.PrintWriter
import java.math.MathContext.DECIMAL128
import scala.annotation.tailrec
import scala.collection.mutable.{Map as MMap, Set as MSet}
import scala.math.{BigInt as SBigInt}
import scala.util.{Try, Success, Failure}

import scala.collection.immutable.SortedMap

/** extensible helper of IR interpreter with a CFG */
class PartialEvaluator(
  val program: Program,
  val log: Boolean = false,
  val detail: Boolean = false,
  val simplifyLevel: Int = 1,
  val logPW: Option[PrintWriter] = None,
  val timeLimit: Option[Int] = None,
  val renamer: Renamer,
  // NOTE : renamer should NOT be copied when copying PState - renamer is, specializer-level global state.
) {
  import PartialEvaluator.*

  /** given spec */
  given Spec = program.spec

  lazy val funcMap = Map.from(program.funcs.map(p => p.name -> p))
  lazy val getFunc = (fname: String) =>
    funcMap.getOrElse(fname, throw UnknownFunc(fname))

  def peval(ref: Ref, pst: PState)(using
    MSet[(Var, Var)],
  ): (Predict[RefTarget], Ref) = ref match

    case x: Var =>
      val newVar = renamer.get(x, pst.context)
      (Known(VarTarget(newVar)), newVar)
    case Field(ref, expr) =>
      val (base, newBase) =
        val (tgt, newRef) = peval(ref, pst);
        (pst(tgt), newRef)
      val (field, newField) = peval(expr, pst)
      val tgt = (base, field) match
        case (Known(b), Known(f)) => Known(FieldTarget(b, f))
        case _                    => (Unknown)
      (tgt, Field(newBase, newField))

  def peval(expr: Expr, pst: PState)(using
    // ad-hoc fix for EClo
    captures: MSet[(Var, Var)],
  ): (Predict[Value], Expr) =
    val result = expr match
      case ERef(ref) =>
        val (tgt, newRef) = peval(ref, pst);
        pst(tgt) match
          case Known(v) if v.isLiteralValue => (pst(tgt), v.toExpr)
          case _                            => (pst(tgt), ERef(newRef))
      // TODO : how to properly handle control flow?

      // case EClo(fname, captured) =>
      // val func = cfg.getFunc(fname)
      // (PClo(func, captured.map(x => x -> pst(x)).toMap), expr)

      case EClo(fname, captured) =>
        val func = getFunc(fname)
        // keep original name `x`
        val v = PClo(
          func,
          captured.map(x => x -> pst(renamer.get(x, pst.context))).toMap,
        )
        captures ++= captured.map(old => old -> renamer.get(old, pst.context))
        (Known(v), EClo(fname, captured))

      case e: LiteralExpr =>
        e match
          case EMath(n)        => (Known(Math(n)), e)
          case EInfinity(pos)  => (Known(Infinity(pos)), e)
          case ENumber(double) => (Known(Number(double)), e)
          case EBigInt(bigInt) => (Known(BigInt(bigInt)), e)
          case EStr(str)       => (Known(Str(str)), e)
          case EBool(b)        => (Known(Bool(b)), e)
          case EUndef()        => (Known(Undef), e)
          case ENull()         => (Known(Null), e)
          case EEnum(name)     => (Known(Enum(name)), e)
          case ECodeUnit(c)    => (Known(CodeUnit(c)), e)
      case EList(exprs) =>
        val vs = exprs.map(expr => peval(expr, pst))
        val addr = renamer.newAddr
        pst.allocList(addr, vs.map(_._1).toVector)
        (
          Known(addr),
          EList(
            vs.map((pv, newExpr) =>
              pv match
                case Known(value) => value.toExprOpt.getOrElse(newExpr)
                case Unknown      => newExpr,
            ),
          ),
        )

      case ERecord(tname, pairs) =>
        val addr = renamer.newAddr
        val pvs = for {
          (f, expr) <- pairs
          (pv, newExpr) = peval(expr, pst)
        } yield (f -> pv, f -> newExpr)
        pst.allocRecord(
          addr,
          tname,
          pvs.map(_._1),
        )
        (
          Known(addr),
          ERecord(
            tname,
            pvs.map {
              case ((key, pv), (_, newExpr)) =>
                pv match
                  case Known(value) => key -> value.toExprOpt.getOrElse(newExpr)
                  case Unknown      => key -> newExpr
            },
          ),
        )
      case ESizeOf(expr) =>
        val (pv, newE) = peval(expr, pst)
        pv match
          case Unknown => (Unknown, ESizeOf(newE))
          case Known(v) =>
            v match
              case addr: Addr =>
                pst(addr) match
                  case Unknown => (Unknown, ESizeOf(newE))
                  case Known(po) =>
                    val ret = Math(po.size)
                    (Known(ret), ret.toExpr)
              case Str(s) =>
                val ret = Math(s.length)
                (Known(ret), ret.toExpr)
              case AstValue(ast) =>
                val ret = Math(ast.children.size)
                (Known(ret), ret.toExpr)
              case v => throw InvalidSizeOf(v)

      case EUnary(uop, expr) =>
        val (pv, newExpr) = peval(expr, pst)
        pv match
          case Known(v) =>
            val result = Interpreter.eval(uop, v)
            (
              Known(result),
              result.toExprOpt match
                case None        => EUnary(uop, newExpr)
                case Some(value) => value,
            )
          case Unknown => (Unknown, EUnary(uop, newExpr))

      case EBinary(BOp.And, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        lv match
          case Known(Bool(false)) => Known(Bool(false)) -> EBool(false)
          case _ =>
            val (rv, newRight) = peval(right, pst)
            (lv, rv) match
              case (Known(lvv), Known(rvv)) =>
                val result = Interpreter.eval(BOp.And, lvv, rvv)
                Known(result) ->
                result.toExprOpt.getOrElse(EBinary(BOp.And, newLeft, newRight))
              case _ =>
                (Unknown, EBinary(BOp.And, newLeft, newRight))

      case EBinary(BOp.Or, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        lv match
          case Known(Bool(true)) => Known(Bool(true)) -> EBool(true)
          case _ =>
            val (rv, newRight) = peval(right, pst)
            (lv, rv) match
              case (Known(lvv), Known(rvv)) =>
                val result = Interpreter.eval(BOp.Or, lvv, rvv)
                Known(result) ->
                result.toExprOpt.getOrElse(EBinary(BOp.Or, newLeft, newRight))
              case _ =>
                (Unknown, EBinary(BOp.Or, newLeft, newRight))
      case EBinary(bop, left, right) =>
        val (lv, newLeft) = peval(left, pst)
        val (rv, newRight) = peval(right, pst)
        (lv, rv) match
          case (Known(v1), Known(v2)) =>
            val result = Interpreter.eval(bop, v1, v2)
            (
              Known(result),
              result.toExprOpt.getOrElse(EBinary(bop, newLeft, newRight)),
            )
          case _ => (Unknown, EBinary(bop, newLeft, newRight))

      case ETypeCheck(expr, ty) =>
        val (pv, newExpr) = peval(expr, pst);
        val ret = ty.ty.contains(pv, pst).map(Bool.apply)
        (ret, ret.map((_: Value).toExpr).getOrElse(ETypeCheck(newExpr, ty)))

      case EContains(list, elem) =>
        val (l, newL) = peval(list, pst)
        val (e, newE) = peval(elem, pst) // TODO is this order okay
        l match
          case Unknown => (Unknown, EContains(newL, newE))
          case Known(addr: Addr) =>
            pst(addr) match
              case Unknown => (Unknown, EContains(newL, newE))
              // TODO make two branch linear
              case Known(lobj: PListObj) if (lobj.values.contains(Unknown)) =>
                (Unknown, EContains(newL, newE))
              case Known(lobj: PListObj) =>
                e match
                  case Unknown => (Unknown, EContains(newL, newE))
                  case Known(ev) =>
                    val ret = Bool(lobj.values.contains(Known(ev)))
                    (Known(ret), ret.toExpr)
              case Known(o) =>
                throw UnexpectedKnown(o, "not a list for EContains")
          case Known(v) => throw UnexpectedKnown(v, "not an addr for EContains")

      // case EParse(code, rule)                       => ???
      // case EGrammarSymbol(name, params)             => ???
      // case ESourceText(expr)                        => ???
      // case EYet(msg)                                => ???
      // case ESubstring(expr, from, to)               => ???
      // case ETrim(expr, isStarting)                  => ???

      // case EVariadic(vop, exprs)                    => ???
      // case EMathOp(mop, args)                       => ???
      // case EConvert(cop, expr)                      => ???
      // case EExists(ref)                             => ???
      // case ETypeOf(base)                            => ???
      // case EInstanceOf(base, target)                => ???
      // case ETypeCheck(base, ty)                     => ???

      // case ECont(fname)                             => ???
      // case EDebug(expr)                             => ???
      // case ERandom()                                => ???
      // case ESyntactic(name, args, rhsIdx, children) => ???
      // case ELexical(name, expr)                     => ???
      // case ERecord(tname, pairs)                    => ???
      // case EMap(ty, pairs)                          => ???
      // case ECopy(obj)                               => ???
      // case EKeys(map, intSorted)                    => ???

      case _ =>
        logging("expr", s"NOT SUPPORTED EXPR! $expr")
        (Unknown, RenameWalker(expr)(using renamer, pst))
    logging("expr", s"$expr -> $result")
    val (v, newE) = result
    if (detail && !v.isKnownLiteral) then
      (v, newE.appendCmt(v.toString(detail = true)))
    else (v, newE)

  def peval(inst: Inst, pst: PState)(using
    captures: MSet[(Var, Var)],
  ): (Inst, PState) =
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      inst.toString(detail = false),
    )
    val (newInst, newPst) = inst match
      case ILet(lhs, expr) =>
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newExpr) = peval(expr, pst)
        pst.define(newLhs, pv)
        (ILet(newLhs, newExpr), pst)
      case ISeq(insts) =>
        val (newInsts, newPst) = insts.foldLeft((List[Inst](), pst)) {
          case ((acc, pst), inst) =>
            val (newInst, newPst) = peval(inst, pst)
            (acc :+ newInst, newPst)
        }
        (ISeq(newInsts), newPst)

      case ISdoCall(lhs, base, method, args) =>
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (pv, newBase) = peval(base, pst)
        val vs = args.map((e) => peval(e, pst)) // TODO check order
        pv match
          case Unknown =>
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst)
          case Known(ast: AstValue) if vs.map(_._1).exists(_.isEmpty) =>
            pst.define(newLhs, Unknown)
            (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst)
          // TODO: local variable inline: <varname>_<fid>_<ctxtcounter>
          // case Known(AstValue(ast)) =>
          case Known(value) =>
            value.asAst match {
              case syn: Syntactic => {
                val (ast0, sdo) = getSdo((syn, method)).getOrElse(
                  throw InvalidAstField(syn, Str(method)),
                )

                val calleeCtx = PContext(
                  func = sdo,
                  locals = MMap.empty,
                  sensitivity = newCallCount,
                  ret = None,
                );

                val baseRep = renamer.newTempCount;
                val subpath = AstHelper.getSubgraphPath(syn, ast0);

                val allocations =
                  (IAssign(Temp(baseRep), newBase)) :: setLocals[PClo](
                    at = calleeCtx,
                    params = sdo.params.map(p =>
                      Param(
                        renamer.get(p.lhs, calleeCtx),
                        p.ty,
                        p.optional,
                        p.specParam,
                      ),
                    ),

                    // TODO : There is no way to print ast0 as expression this should be removed somehow
                    /* Ad-hoc fix */

                    args = (Known(AstValue(ast0)) -> ERef(
                      subpath.foldLeft[Ref](Temp(baseRep))((acc, idx) =>
                        Field(acc, EMath(idx)),
                      ),
                    )) :: vs,
                    func = sdo,
                  )

                val calleePst = {
                  val fresh = pst.copied;
                  fresh.callStack ::= PCallContext(pst.context, newLhs)
                  fresh.context = calleeCtx
                  fresh
                }

                Try {
                  val (body, after) = peval(sdo.body, calleePst)
                  after.callStack match
                    case Nil => /* never */ ???
                    case callerCtx :: rest =>
                      val calleeCtx = after.context;
                      after.callStack = rest
                      after.context = callerCtx.ctxt
                      after.define(
                        newLhs,
                        calleeCtx.ret.getOrElse(Unknown),
                      )
                      (ISeq(List(ISeq(allocations), body)), after)
                }.recoverWith {
                  case NoMoreInline() =>
                    pst.define(newLhs, Unknown)
                    pst.heap.clear(vs.map(_._1)) // ...
                    Success(
                      (ISdoCall(newLhs, newBase, method, vs.map(_._2)), pst),
                    )
                }.get
              }

              case lex: Lexical =>
                val v = Interpreter.eval(lex, method);
                pst.define(newLhs, Known(v))
                (IAssign(newLhs, v.toExpr), pst)
            }
      case call @ ICall(lhs, fexpr, args) =>
        val newCallCount = renamer.newCallCount
        val newLhs = renamer.get(lhs, pst.context)
        val (f, newFexpr) = peval(fexpr, pst)
        val vs = args.map(e => peval(e, pst)) // TODO check order
        f match
          case Known(pclo @ PClo(callee, captured))
              if vs.map(_._1).forall(_.isDefined) =>
            val calleeCtx = PContext(
              func = callee,
              locals = MMap.empty,
              sensitivity = newCallCount,
              ret = None,
            );

            val allocations = setLocals[PClo](
              at = calleeCtx,
              params = callee.params.map(p =>
                Param(
                  renamer.get(p.lhs, calleeCtx),
                  p.ty,
                  p.optional,
                  p.specParam,
                ),
              ),

              // TODO : There is no way to print ast0 as expression this should be removed somehow
              /* Ad-hoc fix */
              args = vs,
              func = callee,
            )

            val calleePst = {
              val fresh = pst.copied;
              fresh.callStack ::= PCallContext(pst.context, newLhs)
              fresh.context = calleeCtx
              fresh
            }

            Try {
              val (body, after) = peval(callee.body, calleePst)
              after.callStack match
                case Nil => /* never */ ???
                case callerCtx :: rest =>
                  val calleeCtx = after.context;
                  after.callStack = rest
                  after.context = callerCtx.ctxt
                  val retVal = calleeCtx.ret.getOrElse(throw NoReturnValue)
                  after.define(newLhs, retVal)
                  (ISeq(List(ISeq(allocations), body)), after)
            }.recoverWith {
              case NoMoreInline() =>
                pst.define(newLhs, Unknown)
                pst.heap.clear(vs.map(_._1))
                Success(ICall(newLhs, newFexpr, vs.map(_._2)), pst)
            }.get

          case Known(_: PClo) /* with Unknown argument */ =>
            pst.define(newLhs, Unknown)
            pst.heap.clear(vs.map(_._1))
            (ICall(newLhs, newFexpr, vs.map(_._2)), pst)
          case Known(_: PCont) => throwPeval"not yet supported"
          case Known(v)        => throw NoCallable(v)
          case Unknown =>
            pst.define(newLhs, Unknown)
            pst.heap.clear(vs.map(_._1))
            (ICall(newLhs, newFexpr, vs.map(_._2)), pst)

      case INop() => (INop(), pst)
      case IAssign(ref, expr) =>
        ref match
          case x: Var =>
            val newVar = renamer.get(x, pst.context);
            val (pv, newExpr) = peval(expr, pst)
            pst.update(newVar, pv)
            (IAssign(newVar, newExpr), pst)
          case Field(_, _) =>
            pst.heap.clear
            (RenameWalker(inst)(using renamer, pst), pst) // TODO

      case IPop(lhs, list, front) =>
        val newLhs = renamer.get(lhs, pst.context);
        val (pv, newListExpr) = peval(list, pst);
        pv match
          case Known(_) => pst.heap.clear // TODO : modify heap
          case Unknown  => pst.heap.clear // TODO : kill heap
        pst.define(newLhs, Unknown)
        (IPop(newLhs, newListExpr, front), pst)

      case IPush(elem, list, front) =>
        val (value, newElem) = peval(elem, pst)
        val (pv, newListExpr) = peval(list, pst)
        pv match
          case Known(addr: Addr) => pst.heap.push(addr, value, front)
          case Unknown           => pst.heap.clear // TODO : kill heap
          case _                 => throwPeval"not an address"
        (IPush(newElem, newListExpr, front), pst)

      case IReturn(expr) =>
        val (pv, newExpr) = peval(expr, pst)
        pst.callStack match
          case head :: _ =>
            pst.context.ret match
              case None =>
                pst.context.ret = Some(pv)
                (
                  IAssign(head.retId, newExpr)
                    .appendCmt(s"<~ IReturn @ ${pst.context.func.name}"),
                  pst,
                )
              case Some(value) =>
                // In this case - we already have one
                // issue : StatementListItem[1,0].TopLevelVarDeclaredNames
                // XXX : path condition might be better
                throw NoMoreInline()

          case Nil =>
            pst.context.ret = Some(pv)
            (IReturn(newExpr), pst)

      case iif @ IIf(cond, thenInst, elseInst) =>
        val (pv, newCond) = peval(cond, pst)
        pv match
          case Known(Bool(true))                 => peval(thenInst, pst)
          case Known(Bool(false))                => peval(elseInst, pst)
          case Known(v)                          => throw NoBoolean(v)
          case Unknown if pst.callStack.size > 0 => throw NoMoreInline()
          case Unknown /* pst.callStack.size == 0 */ => {

            val (thenPst, elsePst) = (pst.copied, pst.copied)
            val (newThen, newThenPst) = peval(thenInst, thenPst)
            val (newElse, newElsePst) = peval(elseInst, elsePst)
            val newPst = newThenPst.join(elsePst)
            (IIf(newCond, newThen, newElse), newPst)

            /* Handle Return */
          }
      case IExpr(expr) => (IExpr(peval(expr, pst)._2), pst)
      case IExpand(_, _) | IDelete(_, _) => /* heap modifying insts */
        val newInst = RenameWalker(inst)(using renamer, pst)
        pst.heap.clear
        (newInst.appendCmt("not supported yet"), pst)

      case IAssert(expr) =>
        val (pv, newExpr) = peval(expr, pst);
        val newInst = IAssert(newExpr);
        pv match
          case Known(v) =>
            v.asBool match
              case true  => (INop(), pst)
              case false => throw AssertionFail(expr)
          case Unknown =>
            (newInst.appendCmt("will be checked at runtime."), pst)

      case IPrint(expr) =>
        val (_, newExpr) = peval(expr, pst);
        val newInst = IPrint(newExpr);
        (newInst, pst)

      case iwhile @ IWhile(cond, body) =>
        val (cv, _) = peval(cond, pst)
        cv match
          case Known(Bool(false)) => (INop(), pst)
          case Known(Bool(true)) =>
            val (newBody, newPst) = peval(body, pst)
            val (rest, newPst2) = peval(iwhile, newPst)
            (ISeq(newBody :: rest :: Nil), newPst2)
          case Known(v) => throw NoBoolean(v)
          case Unknown =>
            val affectedLocals =
              LocalImpact(body)(using renamer, pst.context)
            for { l <- affectedLocals } pst.define(l, Unknown)
            pst.heap.clear
            (
              RenameWalker(inst)(using renamer, pst).appendCmt(
                "unknown condition : heap is cleared",
              ),
              pst,
            )

    // case _ => RenameWalker(inst)(using renamer, pst) -> pst

    logging("pst", pst)
    logging(
      s"inst @ ${pst.context.func.name} = cs[${pst.callStack.size}]",
      s"${inst.toString(detail = false)} -> ${newInst.toString(detail = true).replace("\n", " ")}\n",
    )
    captures.size match
      case 0 => (newInst, newPst)
      case _ =>
        val assigns = ISeq(captures.toList.map {
          case (old -> newV) => IAssign(old, ERef(newV))
        })
        captures.clear
        (ISeq(assigns :: newInst :: Nil), newPst)

  /** final state */
  def run(
    func: Func,
    pst: PState,
    newName: Option[String] = None,
  ): (Func, PState) = timeout(
    {
      val inst = func.body
      val newParams = func.params.map {
        case Param(lhs, ty, optional, specParam) =>
          Param(renamer.get(lhs, pst.context), ty, optional, specParam)
      }
      val result @ (newBody, newPst) = peval(inst, pst)(using MSet.empty)
      val simplifiedNewBody = simplifyLevel match
        case 0 => newBody
        case 1 => InstFlattener(newBody)
        case 2 => InstFlattener(RemoveUnusedDef(newBody))
        case 3 => ??? // InstFlattener(NoLiterals(newBody)) // TODO

      (
        Func(
          func.main,
          func.kind,
          newName.getOrElse(func.name),
          newParams,
          func.retTy,
          simplifiedNewBody,
          func.algo,
        ),
        newPst,
      )
    },
    timeLimit,
  )

  /** ECMAScript parser */
  lazy val esParser: ESParser = program.esParser

  /** get initial local variables */
  def setLocals[T <: PCallable](
    at: PContext,
    params: List[Param],
    args: List[(Predict[Value], Expr)],
    func: Func,
  )(using ev: T <:< PCont = null): List[Inst] = {
    val map = at.locals;
    @tailrec
    def aux(
      evalArgs: List[Inst],
    )(ps: List[Param], as: List[(Predict[Value], Expr)]): List[Inst] =
      (ps, as) match {
        case (Nil, Nil) => (evalArgs)
        case (Param(lhs, ty, optional, _) :: pl, Nil) =>
          if (optional) aux(evalArgs)(pl, Nil)
          else throw RemainingParams(ps)
        case (Nil, args) =>
          // XXX Handle GeneratorStart <-> GeneratorResume arith mismatch
          if (ev != null) then (evalArgs) else ??? // throw RemainingArgs(args)
        case (param :: pl, (arg, argExpr) :: al) =>
          map += (param.lhs -> arg)
          aux(
            IAssign(param.lhs, argExpr) :: evalArgs,
          )(
            pl,
            al,
          )
      }
    // reverse needed to keep order
    aux(Nil)(params, args).reverse.toList
  }

  private def buildLogger = (writer: PrintWriter) =>
    (tag: String, data: Any) =>
      if (log)
        writer.println(s"[$tag] $data")
        writer.flush()

  lazy val logging = buildLogger(pw)

  // ---------------------------------------------------------------------------
  // private helpers
  // ---------------------------------------------------------------------------

  /** type model */
  private def tyModel = program.tyModel

  /** spec */
  private def spec = program.spec

  /** itereration count */
  private var iter = 0

  /** logging */
  private lazy val pw: PrintWriter =
    logPW.getOrElse(getPrintWriter(s"$PEVAL_LOG_DIR/log"))

  /** cache to get syntax-directed operation (SDO) */
  private val getSdo =
    cached[(Ast, String), Option[(Ast, Func)]](
      _.getSdo[Func](_)(using spec, funcMap),
    )

}

/** IR PartialEvaluator with a CFG */
object PartialEvaluator {

  /** prepare new Renamer and PState for partial evaluation */
  def prepare(func: Func): (Renamer, PState) = {
    val renamer = Renamer()
    val thisCallCount = renamer.newCallCount
    val pst = PState.empty(PContext.empty(func, sensitivity = thisCallCount))
    (renamer, pst)
  }

  object ForECMAScript {

    def pevalForOCEB[B[_] <: Iterable[_]](
      program: Program,
      decls: Iterable[(ESHelper.ESFuncAst, Option[String])],
    ): Iterable[(Func, ESHelper.ESFuncAst)] = {
      // TODO can we optimize this?
      val target = program.funcs
        .find(_.name == ORDINARY_CALL_EVAL_BODY)
        .getOrElse(
          throw PartialEvaluatorError(
            s"`prepareOCEB` is only callable with  ${ORDINARY_CALL_EVAL_BODY}",
          ),
        )

      decls.map {
        case (decl, newName) =>
          pevalForOCEB(program, target, decl, newName)
      }
    }

    def pevalForOCEB(
      program: Program,
      decl: ESHelper.ESFuncAst,
      newName: Option[String] = None,
    ): (Func, ESHelper.ESFuncAst) = {
      // TODO can we optimize this?
      val target = program.funcs
        .find(_.name == ORDINARY_CALL_EVAL_BODY)
        .getOrElse(
          throw PartialEvaluatorError(
            s"`prepareOCEB` is only callable with  ${ORDINARY_CALL_EVAL_BODY}",
          ),
        )

      pevalForOCEB(program, target, decl, newName)
    }

    private def pevalForOCEB(
      program: Program,
      target: Func,
      decl: ESHelper.ESFuncAst,
      newName: Option[String],
    ): (Func, ESHelper.ESFuncAst) = {
      val (renamer, pst) =
        PartialEvaluator.ForECMAScript.prepareForOCEB(target, decl);

      val peval = PartialEvaluator(
        program = program,
        renamer = renamer,
        simplifyLevel = 2,
      )

      val pevalResult = Try(
        peval.run(target, pst, newName),
      ).map(_._1)

      pevalResult.map((_, decl)) match
        case Success(v)         => v
        case Failure(exception) => throw exception
    }

    /** create new PState for p-evaluating `OrdinaryCallEvaluateBody`
      *
      * @param func
      *   ir function, must be `FunctionDeclarationInstantation`.
      * @param esFuncDecl
      *   es function declaration to use as an argument.
      * @return
      *   (renamer, pst, params: AstValue, funcBody : AstValue)
      */
    def prepareForOCEB(
      func: Func,
      esFuncDecl: ESHelper.ESFuncAst,
    ) = {

      if (func.name != ORDINARY_CALL_EVAL_BODY) {
        throw PartialEvaluatorError(
          s"`prepareOCEB` is only callable with  ${ORDINARY_CALL_EVAL_BODY}",
        )
      }

      val (renamer, pst) = prepare(func)

      val addr_func_obj_record = renamer.newAddr

      pst.allocRecord(
        addr_func_obj_record,
        esFuncDecl.toRecordTname,
        esFuncDecl.toRecordEntries,
      )

      pst.define(
        renamer.get(Name("F"), pst.context),
        Known(addr_func_obj_record),
      )
      pst.define(
        renamer.get(Name("argumentsList"), pst.context),
        Unknown,
      );

      (renamer, pst)
    }

    def genMap(
      overloads: Iterable[(Func, ESHelper.ESFuncAst)],
    )(using msg: Option[String] = None): SpecializedFuncs = {

      // TODO : optimize finding matching overloads
      val overloadsMap = SortedMap.from(overloads.map {
        case (ol, fa) =>
          (AstValue(fa.params), AstValue(fa.funcBody)) -> ol.name
      })
      // println(
      // s"overloadsMap.size == overloads.size: ${overloadsMap.size == overloads.size}",
      // )
      // println(
      // s"overloadsMap.size, overloads.size: ${overloadsMap.size}, ${overloads.size}",
      // )
      // assert()

      val go =
        (args: Iterable[Value], st: State) =>
          for {
            addr <- args.headOption.flatMap {
              case addr: Addr => Some(addr)
              case _          => None
            }
            record <- st(addr) match
              case r: RecordObj => Some(r)
              case _            => None
            asts <- record
              .get(Str(ESHelper.FORMAL_PARAMS))
              .zip(record.get(Str(ESHelper.ECMASCRIPT_CODE)))
              .flatMap {
                case (v1: AstValue, v2: AstValue) => Some((v1, v2))
                case _                            => None
              }

            // k <- overloadsMap.find(elem => elem._1 == asts).map(_._1)
            // assertion, asts == key then also for .hashCode
            // _ = assert((asts != k) || (asts.hashCode == k.hashCode))
            (fname) <-
              overloadsMap
                .get(asts)
          } yield fname
      // TODO : optimization ?
      val newGo: PartialFunction[(Iterable[Value], State), String] = {
        case (args: Iterable[Value], st: State) if (go(args, st).isDefined) =>
          go(args, st).get
      }
      SpecializedFuncs(
        Map(ORDINARY_CALL_EVAL_BODY -> newGo),
      )
    }

    import math.Ordered.orderingToOrdered

    given [T: Ordering]: Ordering[Option[T]] with
      def compare(x: Option[T], y: Option[T]): Int =
        (x, y) match {
          case (Some(a), Some(b)) => a.compare(b)
          case (Some(_), None)    => 1
          case (None, Some(_))    => -1
          case (None, None)       => 0
        }

    given [T: Ordering]: Ordering[List[T]] with
      def compare(x: List[T], y: List[T]): Int =
        (x.sizeCompare(y)) match
          case 0 =>
            x.zip(y).foldLeft(0) {
              case (acc, (a, b)) =>
                a.compare(b) match
                  case n if acc == 0 => n
                  case _             => acc
            }
          case n => n

    given Ordering[Ast] with
      def compare(x: Ast, y: Ast): Int =
        (x, y) match {
          case (Lexical(n1, s1), Lexical(n2, s2)) =>
            n1.compare(n2) match
              case 0 => s1.compare(s2)
              case n => n
          case (Lexical(_, _), Syntactic(_, _, _, _)) => 1
          case (Syntactic(_, _, _, _), Lexical(_, _)) => -1
          case (Syntactic(n1, a1, r1, c1), Syntactic(n2, a2, r2, c2)) =>
            n1.compare(n2) match
              case 0 =>
                a1.compare(a2) match
                  case 0 =>
                    r1.compare(r2) match
                      case 0 =>
                        c1.compare(c2)
                      case n => n
                  case n => n
              case n => n
        }

    given Ordering[AstValue] with
      def compare(x: AstValue, y: AstValue): Int =
        x.ast.compare(y.ast)
  }
}
