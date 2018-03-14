object ComplexNumber {

  def complexNumberMultiply(a: String, b: String): String = {
    def getRealAndImg(s: String): Array[Int] = s.split("\\+|i").map(_.toInt)

    val A = getRealAndImg(a)
    val B = getRealAndImg(b)
    s"${A(0) * B(0) - A(1) * B(1)}+${A(0) * B(1) + A(1) * B(0)}i"
  }

  def main(args: Array[String]): Unit = {

    val a = "1+-1i"
    val b = "1+-1i"
    val answer = complexNumberMultiply(a, b)

    println(s"The product of $a and $b is $answer")
  }

}
