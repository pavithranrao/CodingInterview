object HammingDistance {

  def hammingDistance(x: Int, y: Int): Int =
    (x ^ y).toBinaryString.count(_ == '1')


  def main(args: Array[String]): Unit = {
    val x = 1
    val y = 4
    println(s"The inputs are $x and $y")

    val answer = hammingDistance(1, 4)
    println(s"The hamming distance is $answer")
  }
}
