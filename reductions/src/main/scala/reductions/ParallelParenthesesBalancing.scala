package reductions

import scala.annotation._
import org.scalameter._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer (new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000
    val chars = new Array[Char](length)
    val threshold = 1000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime")
    println(s"speedup: ${seqtime.value / fjtime.value}")
  }
}

object ParallelParenthesesBalancing extends ParallelParenthesesBalancingInterface {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {

    @tailrec
    def helper(chars: Array[Char], seen: List[Char]): Boolean = chars match {
      case Array() =>
        if (seen.isEmpty) true
        else false
      case Array(')', _*) =>
        if (seen.isEmpty) false
        else helper(chars.tail, seen.tail)
      case Array('(', _*) => helper(chars.tail, ')' :: seen)
      case Array(_*) => helper(chars.tail, seen)
    }

    if (chars.isEmpty) true
    else helper(chars, List())
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, leftBrackets: Int, rightBrackets: Int): (Int, Int) = {
      var i = idx
      var remainingOpen = leftBrackets
      var remClosed = rightBrackets
      while (i < until) {
        val current = chars(i)

        if (current == '(') {
          remainingOpen += 1
        }
        if (current == ')') {
          if (remainingOpen > 0) remainingOpen -= 1
          else remClosed += 1
        }

        i += 1
      }
      (remainingOpen, remClosed)
    }

    def reduce(from: Int, until: Int): (Int, Int) = {

      if (until - from <= threshold) {
        traverse(from, until, 0, 0)
      } else {
        val mid = (from + until) / 2
        val ((leftRemOpen, leftRemClosed), (rightRemOpen, rightRemClosed)) =
          parallel(reduce(from, mid), reduce(mid, until))

        val leftSum = if (leftRemOpen > 0) leftRemOpen + rightRemOpen - rightRemClosed else 0
        val rightSum = if (rightRemOpen > 0) leftRemClosed + rightRemClosed - leftRemOpen else 0
        (leftSum, rightSum)
      }
    }

    val res = reduce(0, chars.length)
    res == (0, 0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
