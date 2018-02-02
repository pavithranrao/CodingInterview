object AddOneToNumber {

  def plusOne(input: Array[Int]): Array[Int] = {
    val accumulator = input.foldRight((Array[Int](), 1)) {
      case (present, (acc, carry)) =>
        ((present + carry) % 10 +: acc, (present + carry) / 10)
    }

    val answer = accumulator._2 +: accumulator._1
    answer.foldLeft(Array[Int]()) {
      (acc, present) =>
        if (present == 0 && acc.isEmpty) {
          acc
        } else {
          acc :+ present
        }
    }
  }


  def main(args: Array[String]): Unit = {
    val input = Array(0, 9)
    println(s"The given input is : (${input.mkString(", ")})")

    val answer = plusOne(input)
    println(s"The answer is : (${answer.mkString(", ")})")
  }
}
