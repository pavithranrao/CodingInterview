import scala.annotation.tailrec

object NumberToBit {


  def numberToBit(num: Int): String = {
    @tailrec
    def _recursive(in: Int, acc: String): String = {
      if (in == 0) {
        acc
      } else {
        if (in % 2 == 1)
          _recursive(in >> 1, s"${acc}1")
        else
          _recursive(in >> 1, s"${acc}0")
      }
    }

    _recursive(num, "")
  }

  def main(args: Array[String]): Unit = {

    val x = 7
    val xBinary = x.toBinaryString
    println(s"Number of ones in $x Binary Representation ($xBinary) is " +
      s"${xBinary.count(_ == '1')}")

    assert(numberToBit(x) == xBinary)

  }
}
