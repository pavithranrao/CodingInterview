object SumCloseToZero {

  def getClosestSumToZero(array: Array[Int]): Option[(Int, Int, Int)] = {

    if (array.length < 2) {
      None
    }

    var left = 0
    var leftVal = array(left)
    var right = array.length - 1
    var rightVal = array(right)
    var absMin = Int.MaxValue

    while (left < right) {
      val newMin = array(right) + array(left)
      if (math.abs(newMin) < absMin) {
        leftVal = array(left)
        rightVal = array(right)
        absMin = newMin
      }

      newMin.compare(0) match {
        case 0 => left = right
        case 1 => right -= 1
        case -1 => left += 1
      }
    }

    Some(leftVal, rightVal, absMin)
  }

  def main(args: Array[String]): Unit = {
    val array = Array(1, 60, -10, 70, -80, 85)
    val comparatorFn = (a: Int, b: Int) => a < b

    QuickSort.genericQuickSort(0, array.length - 1)(array, comparatorFn)
    println(s"The sorted array is ${array.mkString(", ")}")

    val answer = getClosestSumToZero(array)
    if (answer.isDefined) {
      val (left, right, sum) = answer.get
      println(s"The pair with sum closest to zero is : $left and $right " +
        s"and the sum is $sum")
    } else {
      println("The pair with sum closest to zero does not exists")
    }

  }

}
