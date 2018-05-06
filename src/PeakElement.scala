import scala.annotation.tailrec

object PeakElement {

  def main(args: Array[String]): Unit = {

    //    val array = Array(1, 3, 20, 4, 1, 0)
    val array = Array(1, 3, 5, 4, 7, 10, 6)
    // val array = Array(1, 1)
    println(s"The given array is : ${array.mkString(", ")}")

    val answer = findPeakElement(array)
    if (answer.isDefined)
      println(s"The peak element is ${answer.get}")
    else
      println(s"There are no peak elements in the array")

  }

  def findPeakElement(array: Array[Int]): Option[Int] = {
    @tailrec
    def _bst(low: Int, high: Int): Option[Int] = {
      if (low > high) {
        None
      } else {
        val mid = low + (high - low) / 2
        if ((mid == 0 || (array(mid - 1) <= array(mid))) &&
          (mid == array.length - 1 || (array(mid + 1) <= array(mid)))) {
          Some(array(mid))
        } else {
          if (mid > 0 && array(mid - 1) > array(mid))
            _bst(low, mid - 1)
          else
            _bst(mid + 1, high)
        }
      }
    }

    _bst(low = 0, high = array.length - 1)
  }
}
