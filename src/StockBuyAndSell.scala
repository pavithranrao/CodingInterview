object StockBuyAndSell {

  def getMaxDiff(array: Array[Int]): Int = {
    if (array.length <= 1) {
      0
    } else {
      array.tail.foldLeft(0, 0, array.head) {
        case ((maxPrev, maxSoFar, prev), present) =>
          val maxHere = 0 max maxPrev + (present - prev)
          (maxHere, maxSoFar max maxHere, present)
      }._2
    }
  }

  def main(args: Array[String]): Unit = {
    val array = Array(7, 1, 5, 3, 6, 4)
    val answer = getMaxDiff(array)

    println(s"The maximum profit is : $answer")
  }
}
