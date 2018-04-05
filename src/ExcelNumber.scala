object ExcelNumber {
  def main(args: Array[String]): Unit = {
    def titleToNumber(s: String): Int = {
      s.foldLeft(0) {
        case (acc, present) =>
          acc * 26 + present.asDigit - 9
      }
    }

    println(titleToNumber("AA"))
  }

}
