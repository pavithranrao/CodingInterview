object SumOfDigits {

  def main(args: Array[String]): Unit = {

    val answer = addStrings("9", "99")
    println(answer)
  }

  def addStrings(num1: String, num2: String): String = {
    val (total, finalCarry) =
      num1.reverse.zipAll(num2.reverse, '0', '0')
        .foldLeft(("", 0)) {
          case ((acc, carry), (n1, n2)) =>
            // println(s"(acc, carry), (n1, n2) -> ($acc, $carry), ($n1, $n2)")
            val sum = carry + n1.asDigit + n2.asDigit
            val ones = sum % 10
            val newCarry = sum / 10
            (s"$ones$acc", newCarry)
        }

    if (finalCarry == 0) {
      total
    } else {
      s"$finalCarry$total"
    }
  }

}
