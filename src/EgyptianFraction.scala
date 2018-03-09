import scala.annotation.tailrec

object EgyptianFraction {

  type Fraction = (Int, Int)

  def getEgyptianFractions(fraction: Fraction): Array[Fraction] = {

    @tailrec
    def _recursive(input: Fraction, acc: Array[Fraction]): Array[Fraction] = {
      val (num, deNum) = input
      if (num == 0 || deNum == 0) {
        acc
      } else {
        if (deNum % num == 0) {
          acc :+ (1, deNum / num)
        } else if (num > deNum) {
          acc :+ (num / deNum, 1)
        } else {
          val div = (deNum / num) + 1
          _recursive((num * div - deNum, deNum * div), acc :+ (1, div))
        }
      }
    }

    _recursive(fraction, Array[Fraction]())
  }

  def main(args: Array[String]): Unit = {

    // 12/13 => 1/2 + 1/3 + 1/12 + 1/156
    val fraction = (6, 14)
    val answer = getEgyptianFractions(fraction)
    println(s"The fractions are : ${answer.mkString(", ")}")


  }
}
