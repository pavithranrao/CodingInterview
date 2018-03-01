import scala.annotation.tailrec

object MissingTermAP {

  def getMissingTerm(array: Array[Int]): Option[Int] = {

    val diff = (array.last - array.head) / array.length
    val a = array.head

    @tailrec
    def _bst(low: Int, high: Int): Option[Int] = {
      if (low == high) {
        None
      } else {
        val mid = low + (high - low) / 2
        if (array(mid + 1) - array(mid) != diff) {
          Some(array(mid) + diff)
        } else if (mid > 0 && (array(mid) - array(mid - 1) != diff)) {
          Some(array(mid - 1) + diff)
        } else if (array(mid) == a + mid * diff) {
          _bst(mid + 1, high)
        } else {
          _bst(low, mid - 1)
        }
      }
    }

    _bst(0, array.length - 1)
  }

  def main(args: Array[String]): Unit = {

    val array = Array(2, 4, 8, 10, 12, 14)
    println(s"The given AP is : ${array.mkString(", ")}")

    val answer = getMissingTerm(array)
    if (answer.isDefined)
      println(s"The missing number is $answer")
    else
      println("No missing elements in the AP")

  }

}
