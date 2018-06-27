object PlusOne {

  def main(args: Array[String]): Unit = {
    val digits = Array(9, 9, 9)
    val answer = plusOne(digits)

    println(answer.mkString(", "))
  }

  // beats 95%
  def plusOne(digits: Array[Int]): Array[Int] = {
    val finalCarry =
      digits.indices.foldRight(1) {
        case (idx, carry) =>
          val sum = digits(idx) + carry
          val ones = sum % 10
          digits(idx) = ones

          sum / 10
      }

    if (finalCarry == 0)
      digits
    else
      finalCarry +: digits
  }

}
