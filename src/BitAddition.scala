object BitAddition {

  def main(args: Array[String]): Unit = {

    val input1 = "1100011111"
    val input2 = "1000000"
    println(s"The given binary strings ${input1.mkString("")} and ${input2.mkString("")}")

    val (bin1, bin2) = equalize(input1, input2)
    println(s"Balanced binary strings ${bin1.mkString("")} and ${bin2.mkString("")}")
    assert(bin1.lengthCompare(bin2.length) == 0)

    val result = getSum(bin1, bin2)
    println(s"The answer is : $result")

  }

  def getSum(bin1: Seq[Int], bin2: Seq[Int]): String = {
    val answer = bin1.zip(bin2).foldRight((List[Int](), 0)) {
      case ((firstBit, secondBit), (accumulator, carry)) =>
        val sum = firstBit ^ secondBit ^ carry
        // boolean expression for 3-bit addition
        val newCarry = (firstBit & secondBit) | (secondBit & carry) | (firstBit & carry)
        // println(s"$firstBit + $secondBit + $carry => $newCarry$sum")
        (sum +: accumulator, newCarry)
    }
    val result = if (answer._2 == 1) {
      1 +: answer._1
    } else {
      answer._1
    }
    result.mkString("")
  }

  def equalize(input1: String, input2: String): (Seq[Int], Seq[Int]) = {
    input1.zipAll(input2, '0', '0').map {
      case (x, y) => (x.asDigit, y.asDigit)
    }.unzip
  }


  //  def equalize(input1: String, input2: String): (Seq[Int], Seq[Int]) = {
  //
  //    val len1 = input1.length
  //    val len2 = input2.length
  //
  //    val result = len1.compare(len2) match {
  //
  //      case 0 => (input1, input2)
  //      case 1 => (input1, s"${"0" * (len1 - len2)}$input2")
  //      case -1 => (s"${"0" * (len2 - len1)}$input1", input2)
  //    }
  //
  //    (result._1.map(_.asDigit), result._2.map(_.asDigit))
  //
  //  }
}
