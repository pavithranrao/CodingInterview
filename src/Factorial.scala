import scala.annotation.tailrec

object Factorial {

  def factorial(n: Long): Long = {
    @tailrec
    def _recursive(x: Long, product: Long = 1L): Long = {
      x match {
        case 1L | 0L => product
        case _ => _recursive(x - 1, product * x)
      }
    }

    _recursive(n)
  }

  def main(args: Array[String]): Unit = {

    val x = 10

    //    val newAnswer = (1 to x).foldLeft(1)(_ * _)
    val newAnswer = (1 to x).map(_.toLong).product

    val answer = factorial(x)
    println(s"The factorial of $x is : $answer")

    val sum = answer.toString.foldLeft(0L) {
      case (acc, present) =>
        acc + present.asDigit
    }
    println(s"The sum of digits in the factorial of $x is : $sum")

    assert(answer == newAnswer)
    assert(sum == 27)

  }
}
