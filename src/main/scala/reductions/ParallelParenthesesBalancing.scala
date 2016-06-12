package reductions

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
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def balance(chars: Array[Char]): Boolean = {
    var balanced = 0;
    for {
      c <- chars
      if c == '(' || c == ')'
    } {
      if (balanced >= 0) {
        if (c == '(')
          balanced = balanced + 1
        else
          balanced = balanced - 1
      }
    }
    balanced == 0
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
    */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx == until) (arg1, arg2)
      else if (chars(idx) == '(') traverse(idx + 1, until, arg1, arg2 + 1)
      else if (chars(idx) == ')') {
        if (arg2 == 0)
          traverse(idx + 1, until, arg1 + 1, arg2)
        else
          traverse(idx + 1, until, arg1, arg2 - 1)
      } else
        traverse(idx + 1, until, arg1, arg2)
    }

    def reduce(from: Int, until: Int): (Int,Int) = {
      if ((until - from) <= threshold) traverse(from, until, 0, 0)
      else {
        val m = (until - from) / 2
        val (res1, res2) = common.parallel(reduce(from, from + m), reduce(from + m, until))
        if (res1._1 == 0 && res2._1 == 0){
          (0, res1._2 + res2._2)
        } else if (res1._2 > res2._1) {
          (res1._1, res1._2 - res2._1 + res2._2)
        } else {
          (res1._1 - res1._2 + res2._1, res2._2)
        }
      }
    }
    reduce(0, chars.length) == (0,0)
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
