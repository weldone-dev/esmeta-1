package esmeta.util

import esmeta.phase.ArgRegex
import esmeta.error.*

/** option kinds
  *
  * @tparam T
  *   the parsing result type
  */
sealed abstract class OptionKind[T] {

  /** postfix string */
  def postfix: String

  /** a list of argument regular expressions */
  def argRegexList(name: String): List[ArgRegex[T]]

  /** completions for value T */
  def completions: Iterable[String]
}

/** boolean options */
case class BoolOption[T](assign: (T, Boolean) => Unit) extends OptionKind[T] {
  def postfix: String = ""
  def argRegexList(name: String): List[ArgRegex[T]] = List(
    (("-" + name + "=").r, "true|false".r, (c, b) => assign(c, b == "true")),
    (("-" + name).r, "".r, (c, _) => assign(c, true)),
    (("-" + name + "=").r, ".*".r, (c, _) => throw ExtraArgError(name)),
  )
  def completions = List("true", "false")
}

/** number options */
case class NumOption[T](
  assign: (T, Int) => Unit,
  prefer: Iterable[Int] = Iterable.empty,
  defaultForEmpty: Option[Int] = None,
) extends OptionKind[T] {
  def postfix: String = "={number}"
  def argRegexList(name: String): List[ArgRegex[T]] = List(
    (("-" + name + "=").r, "-?[0-9]+".r, (c, s) => assign(c, s.toInt)),
    (("-" + name + "=").r, ".*".r, (_, _) => throw NoNumArgError(name)),
    (
      ("-" + name).r,
      "".r,
      (c, _) => assign(c, defaultForEmpty.getOrElse(throw NoNumArgError(name))),
    ),
  )

  def completions = prefer.map(_.toString)
}

/** string options */
case class StrOption[T](
  assign: (T, String) => Unit,
  prefer: Iterable[String] = Iterable.empty,
  defaultForEmpty: Option[String] = None,
) extends OptionKind[T] {
  def postfix: String = "={string}"
  def argRegexList(name: String): List[ArgRegex[T]] = List(
    (("-" + name + "=").r, ".+".r, (c, s) => assign(c, s)),
    (("-" + name + "=").r, ".*".r, (_, _) => throw NoStrArgError(name)),
    (
      ("-" + name).r,
      "".r,
      (c, s) => assign(c, defaultForEmpty.getOrElse(throw NoStrArgError(name))),
    ),
  )

  def completions = prefer

}

/** string list options */
case class StrListOption[T](assign: (T, List[String]) => Unit)
  extends OptionKind[T] {
  def postfix: String = "={string,...,string}"
  def argRegexList(name: String): List[ArgRegex[T]] = List(
    (("-" + name + "=").r, ".+".r, (c, s) => assign(c, s.split(",").toList)),
    (("-" + name + "=").r, ".*".r, (_, _) => throw NoStrArgError(name)),
    (("-" + name).r, "".r, (_, _) => throw NoStrArgError(name)),
  )

  def completions = Nil
}
