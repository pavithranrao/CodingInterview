object SelfDividingNum {

  def selfDividingNumbers(left: Int, right: Int): List[Int] = {
    (left to right).filter {
      num =>
        val numStr = num.toString
        !numStr.contains('0') && numStr.forall(num % _.asDigit == 0)
    }.toList
  }

  def main(args: Array[String]): Unit = {
    val left = 1
    val right = 22
    val answer = selfDividingNumbers(left, right)

    println(answer)
  }

}
