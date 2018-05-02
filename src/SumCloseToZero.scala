import scala.annotation.tailrec

object SumCloseToZero {

  def main(args: Array[String]): Unit = {
    val array = Array(1, 60, -10, 70, -80, 85)
    val comparatorFn = (a: Int, b: Int) => a < b

    QuickSort.genericQuickSort(0, array.length - 1)(array, comparatorFn)
    println(s"The sorted array is ${array.mkString(", ")}")

    val answer = getClosestSumToZero(array)
    if (answer.isDefined) {
      val (sum, left, right) = answer.get
      println(s"The pair with sum closest to zero is : $left and $right " +
        s"and the sum is $sum")
    } else {
      println("The pair with sum closest to zero does not exists")
    }

  }

  def getClosestSumToZero(array: Array[Int]): Option[(Int, Int, Int)] = {

    if (array.length < 2) {
      None
    } else {

      @tailrec
      def _recursive(left: Int, right: Int)
                    (param: (Int, Int, Int)): Option[(Int, Int, Int)] = {
        if (left < right) {
          val (absMin, leftVal, rightVal) = param
          val newMin = array(right) + array(left)
          val newParam =
            if (math.abs(newMin) < absMin) {
              (newMin, array(left), array(right))
            } else {
              (absMin, leftVal, rightVal)
            }
          newMin.compare(0) match {
            case 0 => Some(newParam)
            case 1 => _recursive(left, right - 1)(newParam)
            case -1 => _recursive(left + 1, right)(newParam)
          }
        } else {
          Some(param)
        }
      }

      val left = 0
      val right = array.length - 1
      _recursive(left, right)((Int.MaxValue, array(left), array(right)))

    }
  }

}
