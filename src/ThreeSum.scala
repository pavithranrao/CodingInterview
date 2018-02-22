object ThreeSum {

  def getThreeSum(array: Array[Int]): Option[(Int, Int, Int)] = {

    val init: Option[(Int, Int, Int)] = None

    QuickSort.genericQuickSort(0, array.length - 1)(array)

    // Need to find a functional way out
    (0 to array.length - 2).foldLeft(init) {
      case (acc, idx) =>
        if (acc.isEmpty) {
          var left = idx + 1
          var right = array.length - 1
          var answer = init
          while (left < right) {
            (array(idx) + array(left) + array(right)).compare(0) match {
              case 0 =>
                answer = Some(array(idx), array(left), array(right))
                left = right
              case 1 => right -= 1
              case -1 => left += 1
            }
          }
          answer
        }
        else {
          acc
        }
    }
  }

  def main(args: Array[String]): Unit = {
    val array = Array(0, -1, 2, -3, 1)
    println(s"The given array is : ${array.mkString(", ")}")

    val answer = getThreeSum(array)
    if (answer.isDefined) {
      val (left, mid, right) = answer.get
      println(s"The answer is : $left + $mid + $right = 0")
    } else {
      println(s"No such triplet exists")
    }

  }

}
