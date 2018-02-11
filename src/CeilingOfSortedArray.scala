import scala.annotation.tailrec

object CeilingOfSortedArray {

  def main(args: Array[String]): Unit = {
    val array = Array(1, 2, 8, 10, 12, 19, 21)
    val target = 11

    val answer = ceilingSearch(0, array.length - 1)(array, target)
    if (answer.isDefined) {
      println(s"The ceiling for $target in ${array.mkString(", ")} is ${answer.get}")
    }
    else {
      println(s"The ceiling for $target in ${array.mkString(", ")} does not exist")
    }

  }

  @tailrec
  def ceilingSearch(low: Int, high: Int)
                   (implicit array: Array[Int], target: Int): Option[Int] = {

    if (target <= array(low)) {
      Some(array(low))
    } else if (target > array(high)) {
      None
    } else {
      val mid = (high + low) / 2
      target.compare(array(mid)) match {
        case 0 => Some(mid)
        case 1 => ceilingSearch(mid + 1, high)
        case -1 => ceilingSearch(low, mid - 1)

      }
    }
  }

}
