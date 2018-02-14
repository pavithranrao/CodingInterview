import scala.annotation.tailrec

object FixedPoint {

  def getFixedPoint(array: Array[Int]): Option[Int] = {
    @tailrec
    def _bst(low: Int, high: Int): Option[Int] = {
      if (low < high) {
        val mid = (low + high) / 2
        array(mid).compare(mid) match {
          case 0 => Some(mid)
          case 1 => _bst(low, mid - 1)
          case -1 => _bst(mid + 1, high)
        }
      } else {
        None
      }
    }

    _bst(0, array.length)
  }

  def main(args: Array[String]): Unit = {
    val array = Array(-10, -5, 0, 3, 7)
    // val array = Array(-10, -5, 3, 4, 7, 9)
    val answer = getFixedPoint(array)

    if (answer.isDefined) {
      println(s"The fixed point is ${answer.get}")
    }
    else {
      println(s"The fixed point does not exist")
    }

  }

}
