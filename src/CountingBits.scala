object CountingBits {

  def main(args: Array[String]): Unit = {
    val input = 5

    val answer = countBits(input)
    println(s"The number of bits are : ${answer.mkString(", ")}")
  }

  def countBits(num: Int): Array[Int] = {
    val numBits = Array.fill(num + 1)(0)
    for (i <- 1 to num)
      numBits(i) = numBits(i & i-1) + 1
    numBits
  }

}
