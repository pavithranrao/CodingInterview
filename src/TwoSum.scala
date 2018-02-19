object TwoSum {

  def getTwoSum(array: Array[Int], target: Int): Option[(Int, Int)] = {

    val comparatorFn = (a: Int, b: Int) => a < b
    QuickSort.genericQuickSort(0, array.length - 1)(array, comparatorFn)

    val init: Option[(Int, Int)] = None
    (0 to array.length / 2).foldLeft(init) {
      case (acc, idx) =>
        val answer = BinarySearch
          .bst(array.slice(idx, array.length),
            target - array(idx))
        if (answer.isDefined) {
          Some(array(idx), array(answer.get))
        } else {
          acc
        }
    }

  }

  def main(args: Array[String]): Unit = {
    val array = Array(2, 7, 11, 15)
    val target = 9
    println(s"The given array is ${array.mkString(", ")}")

    val answer = getTwoSum(array, target)
    if (answer.isDefined) {
      val (left, right) = answer.get
      println(s"The pair is $left and $right whose sum is $target")
    } else {
      println("A pair doesnt exists")
    }
  }

}
