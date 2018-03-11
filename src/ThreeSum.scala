import scala.collection.mutable.ListBuffer

object ThreeSum {

  def getThreeSum(array: Array[Int]): List[List[Int]] = {

    //    val init: Option[(Int, Int, Int)] = None
    QuickSort.genericQuickSort(0, array.length - 1)(array)

    // Need to find a functional way out
    (0 to array.length - 2).foldLeft(ListBuffer[List[Int]]()) {
      case (acc, idx) =>
        var left = idx + 1
        var right = array.length - 1
        while (left < right) {
          (array(idx) + array(left) + array(right)).compare(0) match {
            case 0 =>
              acc += List(array(idx), array(left), array(right))
              left += 1
            case 1 => right -= 1
            case -1 => left += 1
          }
        }
        acc
    }.toList.distinct
  }

  def main(args: Array[String]): Unit = {
    //    val array = Array(0, -1, 2, -3, 1)
    val array = Array(-2, 0, 1, 1, 2)
    println(s"The given array is : ${array.mkString(", ")}")

    val answer = getThreeSum(array)
    println(answer.mkString(", "))
    //    if (answer.isDefined) {
    //      val (left, mid, right) = answer.get
    //      println(s"The answer is : $left + $mid + $right = 0")
    //    } else {
    //      println(s"No such triplet exists")
    //    }

  }

}
