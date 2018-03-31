object UniqueMorseCode {

  def uniqueMorseRepresentations(words: Array[String]): Int = {
    val offset = 'a'.toInt
    val morseCode = Array(".-", "-...", "-.-.", "-..",
      ".", "..-.", "--.", "....", "..", ".---", "-.-",
      ".-..", "--", "-.", "---", ".--.", "--.-", ".-.",
      "...", "-", "..-", "...-", ".--", "-..-", "-.--", "--..")
    val seen = words.foldLeft(Set[String]()) {
      case (codesAcc, word) =>
        val code = word.foldLeft("") {
          case (codeAcc, letter) =>
            codeAcc ++ morseCode(letter.toInt - offset)
        }
        codesAcc + code
    }
    seen.size

  }


  def main(args: Array[String]): Unit = {
    val input = Array("gin", "zen", "gig", "msg")
    val answer = uniqueMorseRepresentations(input)

    println(s"$answer")
  }

}
