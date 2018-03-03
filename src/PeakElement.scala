import scala.annotation.tailrec

object PeakElement {

  def findPeakElement(array: Array[Int]): Option[Int] = {
    @tailrec
    def _bst(low: Int, high: Int): Option[Int] = {
      if (low > high) {
        None
      } else {
        val mid = (low + high) / 2
        if (mid == 0 || mid == array.length - 1) {
          Some(array(mid))
        } else {
          (array(mid).compare(array(mid + 1)), array(mid).compare(array(mid - 1))) match {
            case (1, 1) => Some(array(mid))
            case (0 | -1, _) => _bst(low, mid - 1)
            case (_, 0 | -1) => _bst(mid + 1, high)
            case _ => None
          }
        }
      }
    }

    _bst(0, array.length - 1)
  }

  def main(args: Array[String]): Unit = {

    val array = Array(1, 3, 20, 4, 1, 0)
    // val array = Array(1, 1)
    println(s"The given array is : ${array.mkString(", ")}")
    
    val answer = findPeakElement(array)
    if (answer.isDefined)
      println(s"The peak element is ${answer.get}")
    else
      println(s"There are no peak elements in the array")

  }
}
