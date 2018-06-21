object DiagonalDifference {
  // https://www.hackerrank.com/challenges/diagonal-difference/problem
  def main(args: Array[String]): Unit = {

    def diagonalDifference(matrix: Array[Array[Int]]): Int = {
      val length = matrix.length - 1
      val (leadingSum, antiLeadingSum) =
        matrix.indices.foldLeft((0, 0)) {
          case ((x, y), index) =>
            (x + matrix(index)(index), y + matrix(index)(length - index))
        }

      if (leadingSum > antiLeadingSum) {
        leadingSum - antiLeadingSum
      } else {
        antiLeadingSum - leadingSum
      }
    }

    val matrix = Array(
      Array(11, 2, 4),
      Array(4, 5, 6),
      Array(10, 8, -12))
    val result = diagonalDifference(matrix)
    println(result)

  }

}
