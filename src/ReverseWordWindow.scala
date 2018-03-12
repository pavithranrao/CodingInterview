object ReverseWordWindow {

  def reverseStr(str: String, k: Int): String = {
    val pos = List.fill(str.length / k)(k)
    val (rest, result, rev) =
      pos.foldLeft((str, "", true)) {
        case ((remaining, acc, toggle), current) =>
          val (head, tail) = remaining.splitAt(current)
          if (toggle)
            (tail, acc + head.reverse, !toggle)
          else
            (tail, acc + head, !toggle)
      }
    if (rev)
      result + rest.reverse
    else
      result + rest
  }

  def main(args: Array[String]): Unit = {
    val input = "abcdefg"
    val shift = 2
    println(s"The input is $input")

    val answer = reverseStr(input, shift)
    println(s"The answer is $answer")
  }
}
