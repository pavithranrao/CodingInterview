object StringCorrection {

  def main(args: Array[String]): Unit = {
    val input = "--a-a-a-a--"
    val length = 2

    val answer = licenseKeyFormatting(input, length)
    println(answer)
    assert(answer == "AA-AA")
  }

  def licenseKeyFormatting(input: String, length: Int): String = {
    val (ansAcc, ansBuffer) =
      input.foldRight((List[String](), "")) {
        case (present, (acc, buffer)) =>
          val (newAcc, tempBuffer) =
            if (buffer.length == length) {
              (buffer +: acc, "")
            } else {
              (acc, buffer)
            }

          val newBuffer =
            if (present != '-') {
              present + tempBuffer
            } else {
              tempBuffer
            }

          (newAcc, newBuffer)
      }

    if (ansBuffer == "")
      ansAcc.mkString("-").toUpperCase()
    else
      (ansBuffer +: ansAcc).mkString("-").toUpperCase()
  }

}
