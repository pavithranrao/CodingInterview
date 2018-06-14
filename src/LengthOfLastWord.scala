import scala.annotation.tailrec

object LengthOfLastWord {

  def main(args: Array[String]): Unit = {
    val s = "abc def"
    assert(lengthOfLastWord(s) == 3)
    assert(lengthOfLastWord(" ") == 0)
    assert(lengthOfLastWord("a") == 1)
    assert(lengthOfLastWord("a ") == 1)
  }

  // beats 87%
  def lengthOfLastWord(s: String): Int = {
    val len = s.length

    @tailrec
    def helper(idx: Int = len - 1,
               lengthOfLast: Int = 0)
              (implicit sawChar: Boolean = false): Int = {
      if (idx == -1) {
        lengthOfLast
      } else {
        (s(idx), sawChar) match {
          case (' ', true) => lengthOfLast
          case (' ', false) => helper(idx - 1)(sawChar)
          case (_, _) => helper(idx - 1, lengthOfLast + 1)(sawChar = true)
        }
      }
    }

    if (len > 0)
      helper()
    else
      0
  }


}
