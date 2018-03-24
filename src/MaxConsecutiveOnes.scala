object MaxConsecutiveOnes {

  // Kadanes' Algorithm Variant
  def getMaxConsecutiveOnes(nums: Array[Int]): Int =
    nums.foldLeft((0, 0)) {
      case ((maxUpToHere, maxSoFar), present) =>
        if (present == 1) {
          val maxEndingHere = maxUpToHere + 1
          (maxEndingHere, maxEndingHere max maxSoFar)
        } else {
          (0, maxSoFar)
        }
    }._2

  def main(args: Array[String]): Unit = {
    val array = Array(1, 1, 0, 1, 1, 1)
    println(s"The input array is : ${array.mkString(", ")}")

    val answer = getMaxConsecutiveOnes(array)
    println(s"The maximum consecutive one are : $answer")
  }

}
