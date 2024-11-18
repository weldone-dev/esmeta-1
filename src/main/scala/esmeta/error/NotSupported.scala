package esmeta.error

/** not supported errors */
case class NotSupported(reasonPath: NotSupported.ReasonPath[String])
  extends ESMetaError(s"${reasonPath.mkString("/")}", "NotSupported")
object NotSupported:
  type Reason = String
  type ReasonPath[T] = List[T]
  def apply(category: Category)(reasonPath: ReasonPath[String]): NotSupported =
    NotSupported(category.name :: reasonPath)

  /** implicit conversion from reasons to reason paths */
  given Conversion[Reason, ReasonPath[String]] = List(_)

  /** constants for categories of not supported features */
  enum Category(val name: String) {
    case Harness extends Category("harness")
    case Internationalisation extends Category("internationalisation")
    case Annex extends Category("annex")
    case Negative extends Category("negative")
    case NonStrict extends Category("non-strict")
    case Module extends Category("module")
    case Fixture extends Category("fixture")
    case Feature extends Category("feature")
    case Metalanguage extends Category("metalanguage")
    case Type extends Category("type")
    case Long extends Category("long")
    case Wrong extends Category("wrong")
    case YetCategorized extends Category("yet-categorized")
  }
