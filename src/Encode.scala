object Encode {

  def encodeString(input: String): String = {
    val stringBuff = input.foldLeft(("", 0, input.head)) {
      case ((acc, count, prev), present) =>
        if (prev == present) {
          (acc, count + 1, prev)
        } else {
          (s"$acc$count$prev", 1, present)
        }
    }

    stringBuff.productIterator.mkString("")
  }

  def main(args: Array[String]): Unit = {
    val input = "aaabbcccaa"
    println(s"The given string is : $input")

    val answer = encodeString(input)
    println(s"The encoded string is : $answer")
    assert(answer == "3a2b3c2a")
  }
}
