object RepeatingNumbers {

  def printRepeatingNumbers(array: Array[Int]): Unit = {
    import math.abs
    print("The repeating numbers are :")
    for (idx <- array.indices) {
      if (array(abs(array(idx))) >= 0) {
        array(abs(array(idx))) = -array(abs(array(idx)))
      } else {
        print(s" ${abs(array(idx))}")
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Array(1, 2, 3, 1, 3, 6, 6)
    println(s"The given input array is : ${input.mkString(", ")}")
    printRepeatingNumbers(input)
  }

}
