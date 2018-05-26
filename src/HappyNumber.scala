import scala.annotation.tailrec

object HappyNumber {

  def main(args: Array[String]): Unit = {
    val ans = isHappy(19)
    println(ans)
  }

  def isHappy(n: Int): Boolean = {
    @tailrec
    def helper(num: Int,
               sqNumSet: Set[Int] = Set()): Boolean = {
      val sumOfSq = num.toString.toCharArray.foldLeft(0) {
        case (acc, present) =>
          acc + present.asDigit * present.asDigit
      }

      if (sumOfSq == 1) {
        true
      } else {
        if (sqNumSet.contains(sumOfSq)) {
          false
        } else {
          helper(sumOfSq, sqNumSet + sumOfSq)
        }
      }
    }

    helper(n)
  }
}
