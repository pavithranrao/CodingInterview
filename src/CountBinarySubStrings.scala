object CountBinarySubStrings {

  def countBinarySubstrings(s: String): Int = {
    val (answer, previous, present) =
      s.indices.tail.foldLeft((0, 0, 1)) {
        case ((ans, prev, current), idx) =>
          if (s(idx - 1) != s(idx)) {
            (ans + (prev min current), current, 1)
          } else {
            (ans, prev, current + 1)
          }
      }
    answer + (previous min present)
  }


  def main(args: Array[String]): Unit = {
    val s = "00110"
    val answer = countBinarySubstrings(s)

    println(answer)
    assert(answer == 3)
  }

}
