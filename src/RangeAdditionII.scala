object RangeAdditionII {

  def maxCount(m: Int, n: Int, ops: Array[Array[Int]]): Int = {
    val (newM, newN) = ops.foldLeft((m, n)) {
      case ((minX, minY), present) =>
        (minX min present(0), minY min present(1))
    }
    newM * newN
  }

  def main(args: Array[String]): Unit = {

    val m = 3
    val n = 3
    val ops = Array(Array(2, 2), Array(3, 3))

    val answer = maxCount(m, n, ops)
    println(answer)
  }

}
