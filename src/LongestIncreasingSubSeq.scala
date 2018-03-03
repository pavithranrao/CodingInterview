import scala.annotation.tailrec

object LongestIncreasingSubSeq {

  // check for edge cases
  def getLongestIncreasingSubSeq(array: Array[Int]): (Int, Array[Int]) = {
    val length = array.length
    val C = Array.fill(length)(1)

    for (idx <- 1 until length) {
      for (revIdx <- 0 until idx) {
        if (array(idx) >= array(revIdx)) {
          if (C(idx) < 1 + C(revIdx)) {
            C(idx) = 1 + C(revIdx)
          }
        }
      }
    }

    println(C.mkString(", "))

    @tailrec
    def getTrace(idx: Int, acc: Array[Int] = Array[Int]()): Array[Int] = {
      if (C(idx) == 1) {
        array(idx) +: acc
      } else {
        if (C(idx) == C(idx - 1)) {
          getTrace(idx - 1, acc)
        } else {
          getTrace(idx - 1, array(idx) +: acc)
        }
      }
    }

    (C.max, getTrace(C.lastIndexOf(C.max)))
  }

  def main(args: Array[String]): Unit = {

    val array = Array(5, 6, 7, 3, 4, 5, 6, 7, 8, 9, 10, 1, 2)
    // val array = Array(3, 4, -1, 0, 6, 2, 3)
    val (maxLength, subSeq) = getLongestIncreasingSubSeq(array)
    println(s"The maximum length of sub sequence is : $maxLength")
    println(s"The sub sequence is : ${subSeq.mkString(", ")}")

  }

}
