import scala.annotation.tailrec

object DetectCapital {

  def main(args: Array[String]): Unit = {
    //    val word = "FFFf"
    val word = "Leetcode"
    assert(detectCapitalUse(word))
  }

  // simpler implementation
  //  def detectCapitalUse(word: String): Boolean = {
  //    val capitalCount = word.count(_.isUpper)
  //    // All letters in this word are capitals, like "USA".                OR
  //    // All letters in this word are not capitals, like "leetcode".       OR
  //    // Only the first letter in this word is capital if it has more than one letter, like "Google".
  //
  //    capitalCount == word.length || capitalCount == 0 || (capitalCount == 1 && word.head.isUpper)
  //  }

  // tail-recursive implementation
  // with pattern match
  def detectCapitalUse(word: String): Boolean = {
    val len = word.length

    @tailrec
    def helper(idx: Int = 1,
               firstCaps: Boolean = true,
               allCaps: Boolean = true): Boolean = {
      if (idx == len) {
        true
      } else {
        (firstCaps, allCaps, word(idx).isUpper) match {
          case (true, true, true) =>
            // All letters in this word are capitals, like "USA".
            // all the letters so far are capital
            helper(idx + 1)
          case (false, _, false) =>
            // All letters in this word are not capitals, like "leetcode".
            // all the letters so far are small
            helper(idx + 1, firstCaps = false)
          case (true, false, false) =>
            // Only the first letter in this word is capital if it has more than one letter, like "Google".
            // first is capital, so far are small
            helper(idx + 1, allCaps = false)
          case _ =>
            // any other combination is not proper capital use
            false
        }
      }
    }

    len == 1 || helper(firstCaps = word(0).isUpper, allCaps = word(1).isUpper)
  }

}
