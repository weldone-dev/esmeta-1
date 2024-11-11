package esmeta.es.util.fuzzer

import scala.annotation.tailrec
import scala.collection.mutable.Map as MMap
import esmeta.util.BaseUtils.chiSquaredTest
import io.circe.*, io.circe.syntax.*, io.circe.generic.semiauto.*
import esmeta.util.SystemUtils.*

object FSTrieWrapper:
  def fromDir(baseDir: String): FSTrieWrapper =
    given fsTrieConfigDecoder: Decoder[FSTrieConfig] = deriveDecoder
    val config = readJson[FSTrieConfig](f"$baseDir/fstrie-config.json")
    val fsTrieWrapper = FSTrieWrapper(config)
    fsTrieWrapper.replaceRootFromJson(f"$baseDir/fstrie-root.json")
    fsTrieWrapper

  def debug: Unit =
    val trie = FSTrieWrapper(
      FSTrieConfig(promotionCriteria = 1, demotionCriteria = 1),
    )
    import trie.given
    val hitStacks = List(
      List("a"),
      List("a"),
      List("a"),
      List("a"),
    )
    val missStacks = List(
      List("b"),
      List("c"),
      List("d"),
      List("e"),
      List("f"),
      List("g"),
      List("h"),
      List("i"),
      List("j"),
      List("k"),
      List("l"),
      List("m"),
      List("n"),
      List("o"),
      List("p"),
      List("q"),
      List("r"),
      List("s"),
    )
    trie.touchWithHit(hitStacks)
    trie.touchWithMiss(missStacks)
    println(trie(List("a")))
    println(trie(List("b")))
    println(trie.root)

class FSTrieWrapper(
  val config: FSTrieConfig,
  val debug: Boolean = false,
) {
  var root: FSTrie = FSTrie(status = FSTrieStatus.Noticed)

  val scoringFunction: (Int, Int) => Double = (hits, misses) => {
    val absentHits = rootHits - hits
    val absentMisses = rootMisses - misses
    chiSquaredTest(hits, misses, absentHits, absentMisses)
  }

  private var rootHits: Int = 0
  private var rootMisses: Int = 0

  /** Insert feature stacks from a single script into the trie. The script
    * succeeded to invoke some non-trivial minifier operations. Increment the
    * hits of each node in the trie.
    *
    * @param stacks
    *   the feature stacks generated from the successful script
    */
  def touchWithHit(stacks: Iterable[List[String]]): Unit =
    rootHits += stacks.size
    stacks.foreach(root.touchByStack(_, isHit = true))
    root.writeback()
    root.updateStatus()

  /** Insert feature stacks from a single script into the trie. The script
    * failed to invoke some non-trivial minifier operations. Increment the
    * misses of each node in the trie.
    *
    * @param stacks
    *   the feature stacks generated from the failed script
    */
  def touchWithMiss(stacks: Iterable[List[String]]): Unit =
    rootMisses += stacks.size
    stacks.foreach(root.touchByStack(_, isHit = false))
    root.writeback()
    root.updateStatus()

  def apply(stack: List[String]): Int = root(stack)

  given fSTrieEncoder: Encoder[FSTrie] = deriveEncoder
  given fsTrieDecoder: Decoder[FSTrie] = deriveDecoder
  given fsTrieConfigEncoder: Encoder[FSTrieConfig] = deriveEncoder
  given fsTrieConfigDecoder: Decoder[FSTrieConfig] = deriveDecoder

  def replaceRootFromJson(filename: String): Unit =
    root = readJson[FSTrie](filename)

  def stacks: Set[List[String]] = root.stacks

  /** A trie that stores the status of each node in the trie, and calculates the
    * score of each node based on the hits and misses of the node and its
    * children. The score is used to determine the promotion or demotion of a
    * node. The fields are mutable for (maybe) better performance than using
    * immutable case classes and copying.
    *
    * @param children
    * @param status
    * @param hits
    * @param misses
    * @param promotables
    *   number of Promotable descendants of this node.
    * @param avgScore
    *   average score of its descendants which are Promotable. If this node is
    *   Promotable, the score is calculated based on the hits and misses of this
    *   node.
    * @param avgScoreSq
    *   average of the square of the scores of its descendants which are
    *   Promotable If this node is Promotable, the scoreSquared is calculated
    *   based on the hits and misses of this node. This is used to calculate the
    *   standard deviation of the scores
    */
  case class FSTrie(
    private val children: MMap[String, FSTrie] = MMap.empty[String, FSTrie],
    private var status: FSTrieStatus,
    private var hits: Int = 0,
    private var misses: Int = 0,
    private var dirty: Boolean = false,
    private var promotables: Int = 0,
    private var avgScore: Double = 0,
    private var avgScoreSq: Double = 0,
  ) {
    import FSTrieStatus.*

    /** How many features do we need to take from the stack?
      *
      * @param stack
      *   the stack of features' names
      * @return
      *   the number of features we need to take
      */
    private[FSTrieWrapper] def apply(stack: List[String]): Int =
      math.min(
        foldByStack(stack, -1) {
          case (acc, node) =>
            if node.status == Noticed then acc + 1
            else acc
        },
        config.maxSensitivity,
      )

    /** Insert a feature stack into the trie. Increment the hits or misses of
      * each node in the trie based on whether the node is hit or miss.
      */
    @tailrec
    private[FSTrieWrapper] final def touchByStack(
      stack: List[String],
      isHit: Boolean,
    ): Unit =
      if isHit then hits += 1 else misses += 1
      dirty = true
      stack match {
        case Nil =>
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.touchByStack(tail, isHit)
            case None =>
              val child = new FSTrie(
                MMap.empty[String, FSTrie],
                status match {
                  case Noticed => Promotable
                  case _       => Ignored
                },
              )
              children(head) = child
              child.touchByStack(tail, isHit)
          }
      }

    /** Write back the scores and number of promotables of each node if it's
      * dirty
      */
    private[FSTrieWrapper] def writeback(): Unit = foreachFromLeaf { node =>
      // write back the if it's dirty
      if node.dirty then
        node.dirty = false
        node.status match {
          case Promotable => // calculate the score based on hits and misses
            node.avgScore = scoringFunction(node.hits, node.misses)
            node.avgScoreSq = node.avgScore * node.avgScore
            node.promotables = 1
          case Noticed => // calculate the average score and number of the node's promotables
            node.promotables = node.children.valuesIterator
              .map(child => child.promotables)
              .sum
            node.avgScore = node.children.valuesIterator
              .map(child => child.avgScore * child.promotables)
              .sum / node.promotables
            node.avgScoreSq = node.children.valuesIterator
              .map(child => child.avgScoreSq * child.promotables)
              .sum / node.promotables
          case Ignored => ()
        }
    }

    /** Promote or demote each node in the trie based on the score of this node.
      * Assume that the children have already been written back.
      */
    private[FSTrieWrapper] def updateStatus(): Unit =
      val promotionScore = avgScore + config.promotionCriteria * stdev
      val demotionScore = avgScore - config.demotionCriteria * stdev
      // println(f"Promotion score: $promotionScore%.2f")
      // println(f"Demotion score: $demotionScore%.2f")
      // demote to Ignored from the leaf first
      foreachFromLeaf { node =>
        node.status match {
          case Noticed if node.avgScore < demotionScore =>
            node.status = Ignored
          case _ => node.status
        }
      }
      // promote from Promotable to Noticed and Ignored to Promotable
      foreachFromRoot { node =>
        node.status match {
          case Noticed =>
            children.valuesIterator.foreach { child =>
              child.status match
                case Ignored => child.status = Promotable
                case _       =>
            }
          case Promotable if node.avgScore > promotionScore =>
            node.status = Noticed
            node.children.valuesIterator.foreach { c =>
              c.status = Promotable
            }
          case _ => node.status
        }
      }

    /** Recursively do something to each node in the trie, starting from the
      * root
      */
    private def foreachFromRoot(
      op: FSTrie => Unit,
      iterIgnored: Boolean = false,
    ): Unit =
      op(this)
      children.valuesIterator.foreach { child =>
        if iterIgnored || child.status != Ignored then
          child.foreachFromRoot(op, iterIgnored)
      }

    /** Recursively do something to each node in the trie, starting from the
      * leaf
      */
    private def foreachFromLeaf(
      op: FSTrie => Unit,
      iterIgnored: Boolean = false,
    ): Unit =
      children.valuesIterator.foreach { child =>
        if iterIgnored || child.status != Ignored then
          child.foreachFromRoot(op, iterIgnored)
      }
      op(this)

    @tailrec
    private final def iterByStack(stack: List[String])(
      op: FSTrie => Unit,
    ): Unit =
      op(this)
      stack match {
        case Nil =>
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.iterByStack(tail)(op)
            case None        =>
          }
      }

    @tailrec
    private final def foldByStack[A](stack: List[String], z: A)(
      op: (A, FSTrie) => A,
    ): A =
      stack match {
        case Nil => op(z, this)
        case head :: tail =>
          children.get(head) match {
            case Some(child) => child.foldByStack(tail, op(z, this))(op)
            case None        => z
          }
      }

    private def get(stack: List[String]): Option[FSTrie] =
      foldByStack(stack, Option.empty[FSTrie])((_, node) => Some(node))

    private def touches: Int = hits + misses

    private def stdev: Double = Math.sqrt(avgScoreSq - avgScore * avgScore)

    def stacks = stacksSuppl(Nil, Set.empty)

    private def stacksSuppl(
      currStack: List[String],
      acc: Set[List[String]],
    ): Set[List[String]] =
      if children.isEmpty then acc + currStack
      else if (status == Promotable) || (status == Ignored) then acc
      else
        children.flatMap {
          case (k, v) =>
            v.stacksSuppl(currStack :+ k, acc)
        }.toSet + currStack
  }
}

case class FSTrieConfig(
  promotionCriteria: Int = 3, // 3 sigma for promotion by default
  demotionCriteria: Int = 3, // 3 sigma for demotion by default
  maxSensitivity: Int = 3,
)

/** Status of a node in the trie
  *   - Noticed: actively noticed; will not be demoted
  *   - Promotable: ignored but might be promoted to Noticed
  *   - Ignored: not noticed; will not be promoted
  */
enum FSTrieStatus:
  case Noticed
  case Promotable
  case Ignored
