package esmeta.peval

import esmeta.cfg.CFG
import esmeta.cfgBuilder.CFGBuilder
import esmeta.error.{PartialEvaluatorError}
import esmeta.es.*
import esmeta.ir.*
import esmeta.peval.pstate.PState
import esmeta.peval.util.*
import esmeta.state.*
import esmeta.util.{ProgressBar, ConcurrentPolicy}
import esmeta.util.BaseUtils.{time}
import java.io.PrintWriter
import math.Ordered.orderingToOrdered
import scala.concurrent.duration.Duration
import scala.collection.immutable.SortedMap
import scala.util.Try

/** Helper for IR PartialEvaluator using ES knowledge */
object ESPartialEvaluator {

  lazy val TARGET = ORDINARY_CALL_EVAL_BODY

  def pevalThenConstruct(
    program: Program,
    decls: List[(ESHelper.ESFuncAst, Option[String])],
  )(
    log: Option[PrintWriter] = None,
    detail: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
    simplifyLevel: Int = 1,
    cp: ConcurrentPolicy = ConcurrentPolicy.Single,
    verbose: Boolean = false,
  ): Program = {
    val (overloadByAst, overloadByName) = peval(
      program = program,
      decls = decls,
    )(log, detail, timeLimit, simplifyLevel, cp, verbose)
    val sfMap = genMap(overloadByAst)
    val newProg = Program(
      overloadByAst.map(_._1) ::: overloadByName ::: program.funcs,
      program.spec,
    )
    newProg.sfMap = sfMap
    newProg
  }

  def pevalThenConstructCFG(
    cfg: CFG,
    decls: List[(ESHelper.ESFuncAst, Option[String])],
  )(
    log: Option[PrintWriter] = None,
    detail: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
    simplifyLevel: Int = 1,
    cp: ConcurrentPolicy = ConcurrentPolicy.Single,
    verbose: Boolean = false,
  ): CFG = {
    val (overloadByAst, overloadByName) = peval(
      program = cfg.program,
      decls = decls,
    )(log, detail, timeLimit, simplifyLevel, cp, verbose)
    val sfMap = genMap(overloadByAst)

    // TODO log execution time
    val (duration, newCfg) = time {
      CFGBuilder
        .byIncremental(cfg, overloadByAst.map(_._1) ::: overloadByName, sfMap)
        .getOrElse(???) // dszxCfg incremental build fail
    }
    newCfg
  }

  def genMap(
    overloads: Iterable[(Func, ESHelper.ESFuncAst)],
  ): SpecializedFuncs = {

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
      Map(TARGET -> newGo),
    )
  }

  /* private helpers  */

  /** peval multiple overloads
    *
    * @return
    *   (List[(Func, ESFuncAst)], List[Func])
    * @notes
    *   first list is the target overloads, second list is the forked overloads
    *   forked overloads are called by its name, specialized check is not needed
    */
  def peval[B[_] <: Iterable[_]](
    program: Program,
    decls: List[(ESHelper.ESFuncAst, Option[String])],
  )(
    logPW: Option[PrintWriter] = None,
    detailPW: Option[PrintWriter] = None,
    timeLimit: Option[Int] = None,
    simplifyLevel: Int = 1,
    cp: ConcurrentPolicy = ConcurrentPolicy.Single,
    verbose: Boolean = false,
  ): (List[(Func, ESHelper.ESFuncAst)], List[Func]) = {
    // TODO can we optimize this?
    val target = program.funcs
      .find(_.name == TARGET)
      .getOrElse(
        throw PartialEvaluatorError(
          s"`ESPartialEvaluator.peval` is only callable with  ${TARGET}",
        ),
      )

    val progress = ProgressBar(
      "Partial Evaluation",
      decls,
      concurrent = cp,
      timeLimit = timeLimit,
      verbose = verbose,
      detail = false,
    )

    val lst = (for (pair <- progress) yield {
      val (decl, newName) = pair
      Try {
        PartialEvaluator.run(program, target)(prepare(decl))(
          logPW = logPW,
          detailPW = detailPW,
          timeLimit = None,
          simplifyLevel = 2,
        )
      }.map((_, decl)).toOption.get
    }).toList

    val overloadByName = lst.flatMap { case ((_, forks), _) => forks }
    val overloadByAst = lst.map { case ((f, forks), ast) => (f, ast) }
    (overloadByAst, overloadByName)
  }

  private def prepare(esFuncDecl: ESHelper.ESFuncAst)(
    renamer: Renamer,
    pst: PState,
  ): (Renamer, PState) = {
    val addr_func_obj_record = renamer.newAddr

    /* NOTE: globals are not modified, so we can use the same globals for all overloads
  val init = new Initialize(cfg)
  val globals = for {
    (x, v) <- init.initGlobal
    pv = v match
      case _: Addr => Unknown
      case _       => Known(v)
  } yield x -> pv */

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

  private lazy val ORDINARY_CALL_EVAL_BODY = "OrdinaryCallEvaluateBody"

  /* given contexts to map asts */
  // TODO check if is it okay to compare Ast by value (not okay in general, what about functions?)

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
