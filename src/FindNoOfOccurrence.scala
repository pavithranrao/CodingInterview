object FindNoOfOccurrence {

  def main(args: Array[String]): Unit = {

    val array = Array(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6)
    println(s"Given array : ${array.zipWithIndex.mkString(", ")}")

    val target = 3
    val first = binarySearch(array, target, isFirst = true)
    val last = binarySearch(array, target, isFirst = false)
    val occurrence = last - first + 1

    println(s"Starting & ending points of $target are $first and $last" +
      s" and the no. of occurrence are $occurrence")

  }

  def binarySearch(array: Array[Int], target: Int, isFirst: Boolean): Int = {

    var low = 0
    var high = array.length - 1

    var point = -1

    while (low <= high) {
      val mid = (low + high) / 2
      array(mid).compare(target) match {
        case -1 =>
          low = mid + 1
        case 0 =>
          point = mid
          if (isFirst)
            high = mid - 1 // First Occurrence
          else
            low = mid + 1 // Last Occurrence
        case 1 =>
          high = mid - 1
      }
    }
    point
  }

}
