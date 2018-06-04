object BannedWords {

  def main(args: Array[String]): Unit = {
    val paragraph = "Bob hit a ball, the hit BALL flew far after it was hit."
    val banned = Array("hit")
    assert(mostCommonWord(paragraph, banned) == "ball")
  }

  def mostCommonWord(paragraph: String, banned: Array[String]): String = {
    val bannedSet = banned.toSet
    val wordFreq = Util.getFreqMap(paragraph
      .replaceAll("\\pP", "")
      .toLowerCase()
      .split("\\s+"))

    wordFreq.foldLeft(("", 0)) {
      case ((acc, count), (word, freq)) =>
        if (!bannedSet.contains(word) && freq > count) {
          (word, freq)
        } else {
          (acc, count)
        }
    }._1

  }

}
