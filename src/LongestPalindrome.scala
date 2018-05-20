object LongestPalindrome {

  def main(args: Array[String]): Unit = {
    val answer = longestPalindrome("abccccdd")
    println(answer)
  }

  def longestPalindrome(s: String): Int = {

    // get the odd # of occurring elements from the input string
    val oddElements = s.foldLeft(Set[Char]()) {
      case (acc, present) =>
        if (acc.contains(present)) {
          acc - present
        } else {
          acc + present
        }
    }

    if (oddElements.isEmpty) {
      // all the chars have a pair
      // the length of the string is the length of the longest palindrome
      s.length
    } else {
      // can have one char in the middle that has no pair
      // hence + 1
      s.length - oddElements.size + 1
    }
  }

}
