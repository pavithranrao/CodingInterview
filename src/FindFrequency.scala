object FindFrequency {

  def getFrequency(array: Array[Int]): Array[Int] = {
    // Can also be achieved by using Map[Int, Int]
    val freqArray = Array.fill(array.max + 1)(0)

    def _bst(low: Int, high: Int): Unit = {
      if (array(low) == array(high)) {
        freqArray(array(low)) += (high - low + 1)
      } else {
        val mid = (low + high) / 2
        _bst(low, mid)
        _bst(mid + 1, high)
      }
    }

    _bst(0, array.length - 1)
    freqArray
  }

  def main(args: Array[String]): Unit = {
    val array = Array(1, 1, 1, 2, 3, 3, 5, 5, 8, 8, 8, 9, 9, 10)

    val answer = getFrequency(array)
    answer.zipWithIndex.foreach {
      case (freq, num) =>
        println(s"The number $num has appeared $freq time(s).")
    }
  }
}
