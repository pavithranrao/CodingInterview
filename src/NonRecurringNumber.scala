import scala.annotation.tailrec

object NonRecurringNumber {

  def getNonRecurringNumber(array: Array[Int]): Option[Int] = {

    @tailrec
    def _bst(low: Int, high: Int): Option[Int] = {
      if (low > high) {
        None
      } else {
        val mid = (low + high) / 2
        if (array(mid) == array(mid - 1)) {
          if ((mid - 1) % 2 == 0)
            _bst(mid + 1, high)
          else
            _bst(low, mid - 1)
        } else if (array(mid) == array(mid + 1)) {
          if (mid % 2 == 0)
            _bst(mid + 1, high)
          else
            _bst(low, mid - 1)
        } else {
          Some(array(mid))
        }
      }
    }

    _bst(0, array.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val array = Array(7, 7, 2, 2, 3, 3, 4, 5, 5, 1, 1)
    println(s"The given array is ${array.mkString(", ")}")

    val answer = getNonRecurringNumber(array)
    if (answer.isDefined)
      println(s"The non recurring number is ${answer.get}")
    else
      println(s"There are no non recurring number in the array")
  }

}
