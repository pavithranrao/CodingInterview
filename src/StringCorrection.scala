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
  }

  def getCorrectedString(input: String, length: Int): String = {
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

        val newBuffer = if (present != '-') {
          tempBuffer +
            (if (present.isLetter && present.isUpper) {
              present.toLower
            } else {
              present
            })
        } else {
          tempBuffer
        }

        (newAcc, newBuffer, newCount)
    }

    ansBuffer.reverse + ansAcc.reverse
  }

}
