object NumberOfLines {

  def main(args: Array[String]): Unit = {

    val widths = Array(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
      10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10)
    val input = "abcdefghijklmnopqrstuvwxyz"
    val answer = numberOfLines(widths, input)
    println(answer.mkString(", "))

  }

  def numberOfLines(widths: Array[Int], S: String): Array[Int] = {
    val (numLines, numChars) =
      S.foldLeft((1, 0)) {
        case ((count, acc), present) =>
          val size = widths(present.toInt - 97)
          if (size + acc > 100) {
            (count + 1, size)
          } else {
            (count, acc + size)
          }
      }
    Array(numLines, numChars)
  }

}
