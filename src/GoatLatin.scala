object GoatLatin {
  def main(args: Array[String]): Unit = {
    val s = "The quick brown fox jumped over the lazy dog"
    val answer = toGoatLatin(s)

    println(answer)
    assert(answer == "heTmaa uickqmaaa rownbmaaaa oxfmaaaaa " +
      "umpedjmaaaaaa overmaaaaaaa hetmaaaaaaaa azylmaaaaaaaaa ogdmaaaaaaaaaa")
  }

  def toGoatLatin(S: String): String = {
    val vowels = Set('a', 'e', 'i', 'o', 'u')
    val vowelsSet = vowels ++ vowels.map(_.toUpper)

    def convert(word: String): String = {
      if (vowelsSet.contains(word.head)) {
        s"${word}ma"
      } else {
        s"${word.tail}${word.head}ma"
      }
    }

    S.split(" ").foldLeft(("", 1)) {
      case ((acc, count), word) =>
        (acc + " " + convert(word) + "a" * count, count + 1)
    }._1.tail
  }

}
