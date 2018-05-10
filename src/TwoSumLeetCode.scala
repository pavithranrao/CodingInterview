import scala.annotation.tailrec

object TwoSumLeetCode {

  def main(args: Array[String]): Unit = {
    val input = Array(3, 2, 4)
    val target = 6

    val answer = twoSum(input, target)
    println(answer.mkString(", "))

    val answer2 = twoSum2(input, target)
    println(answer2.mkString(", "))

  }

  // slow
  // one complete traversal and second traversal with early exit
  def twoSum(input: Array[Int], target: Int): Array[Int] = {
    val numHash =
      input.indices.foldLeft(Map[Int, Int]()) {
        case (acc, idx) =>
          acc.updated(input(idx), idx)
      }

    @tailrec
    def _bs(idx: Int = 0, ans: Array[Int] = Array(-1, -1)): Array[Int] = {
      if (numHash.contains(target - input(idx)) && numHash(target - input(idx)) != idx) {
        Array(idx, numHash(target - input(idx)))
      } else {
        _bs(idx + 1)
      }
    }

    _bs()
  }

  // faster
  // early exit in only one traversal
  def twoSum2(input: Array[Int], target: Int): Array[Int] = {
    @tailrec
    def _bsHash(idx: Int = 0,
                numHash: Map[Int, Int] = Map[Int, Int]()): Array[Int] = {

      val present = input(idx)
      val complement = target - present
      if (numHash.contains(complement)) {
        Array(idx, numHash(complement))
      } else {
        _bsHash(idx + 1,
          numHash.updated(present, idx))
      }

    }

    _bsHash()
  }

}
