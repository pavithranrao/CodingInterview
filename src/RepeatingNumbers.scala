object RepeatingNumbers {

  def printRepeatingNumbers(array: Array[Int]): List[Int] = {
    import math.abs
    array.indices.foldLeft(List[Int]()) {
      case (acc, idx) =>
        val present = abs(array(idx)) - 1
        if (array(present) >= 0) {
          array(present) = -array(present)
          acc
        } else {
          acc :+ abs(present + 1)
        }
    }
  }

  def main(args: Array[String]): Unit = {
    // val input = Array(1, 2, 3, 1, 3, 6, 6)
    val input = Array(4, 3, 2, 7, 8, 2, 3, 1)
    println(s"The given input array is : ${input.mkString(", ")}")

    val answer = printRepeatingNumbers(input)
    println(s"The repeating numbers are  ${answer.mkString(", ")}")
  }

}
