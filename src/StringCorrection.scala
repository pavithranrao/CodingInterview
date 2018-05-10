object StringCorrection {

  def main(args: Array[String]): Unit = {
    val input = "usagfbs-23hb-GKDF2Bumfd-sdf"
    val length = 4

    val answer = getCorrectedString(input, length)
    println(answer)
    assert(answer == "usag-fbs2-3hbg-kdf2-bumf-dsdf")
    val input2 = "ABCDefghIJKLmnop"
    val length2 = 2
    assert(getCorrectedString(input2, length2) == "ab-cd-ef-gh-ij-kl-mn-op")

    val answer2 = getCorrectedString2(input, length)
    println(answer2)
    assert(answer == answer2)

    assert(getCorrectedString(input2, length2) == getCorrectedString2(input2, length2))
  }

  def getCorrectedString(input: String, length: Int): String = {
    // lots of if else
    // could be eliminated if accumulator is a List[String] instead of String
    val (ansAcc, ansBuffer, _) = input.foldRight(("", "", 0)) {
      case (present, (acc, buffer, count)) =>
        //        println(s"(present, (acc, buffer, count)) -> " +
        //          s"($present, ($acc, $buffer, $count))")
        val (newAcc, tempBuffer) =
          if (count == length) {
            (acc + buffer + "-", "")
          } else {
            (acc, buffer)
          }

        val newCount =
          if (count == length) {
            if (present != '-') 1 else 0
          } else {
            if (present != '-') count + 1 else count
          }

        val newBuffer =
          if (present != '-') {
            tempBuffer + (if (present.isUpper) present.toLower else present)
          } else {
            tempBuffer
          }

        (newAcc, newBuffer, newCount)
    }

    ansBuffer.reverse + ansAcc.reverse
  }
  
  def getCorrectedString2(input: String, length: Int): String = {
    val (ansAcc, ansBuffer) = input.foldRight((List[String](), "")) {
      case (present, (acc, buffer)) =>
        val (newAcc, tempBuffer) =
          if (buffer.length == length) {
            (buffer +: acc, "")
          } else {
            (acc, buffer)
          }

        val newBuffer =
          if (present != '-') {
            (if (present.isUpper) present.toLower else present) + tempBuffer
          } else {
            tempBuffer
          }

        (newAcc, newBuffer)
    }

    (ansBuffer +: ansAcc).mkString("-")
  }
}
