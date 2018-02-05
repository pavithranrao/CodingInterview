object Decode {

  def decodeString(input: String): String = {
    input.foldLeft(("", 0)) {
      case ((acc, count), present) =>
        present match {
          case _ if (present >= 48) && (present <= 57) =>
            (acc, count * 10 + present - 48)
          case _ =>
            (s"$acc${present.toString * count}", 0)
        }
    }._1
  }


  def main(args: Array[String]): Unit = {

    val input = "aaaabbccccaa"
    val encodedInput = Encode.encodeString(input)
    println(s"The encoded input is $encodedInput")

    val answer = decodeString(encodedInput)
    println(s"The answer is $answer")

    assert(input == answer)
  }

}
