import scala.annotation.tailrec

object UglyNumber {

  // from LeetCode solutions
  // good use of pattern matching
  @tailrec
  def isUgly2(num: Int): Boolean =
    num match {
      case 2 | 3 | 5 | 1 => true
      case n if n < 1 => false
      case n if n % 2 == 0 =>
        isUgly2(n / 2)
      case n if n % 3 == 0 =>
        isUgly2(n / 3)
      case n if n % 5 == 0 =>
        isUgly2(n / 5)
      case _ => false
    }

  def main(args: Array[String]): Unit = {
    val inputs = Array(6, 8, 14)
    val outputs = inputs.map(isUgly)

    assert(outputs sameElements Array(true, true, false))
  }

  // beats 100%
  def isUgly(num: Int): Boolean = {
    val primes = List(2, 3, 5)

    @tailrec
    def div(n: Int = num): Boolean = {
      if (n == 1) {
        true
      } else {
        val divisor = primes.find(n % _ == 0)
        if (divisor.isDefined)
          div(n / divisor.get)
        else
          false
      }
    }

    num > 0 && div()
  }

}
