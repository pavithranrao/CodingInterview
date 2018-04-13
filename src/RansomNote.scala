object RansomNote {

  def main(args: Array[String]): Unit = {
    val ransomNote = "aa"
    val magazine = "aab"
    val answer = canConstruct(ransomNote, magazine)

    println(answer)
    assert(!canConstruct("aa", "ab"))
    assert(!canConstruct("a", "b"))
  }

  def canConstruct(ransomNote: String, magazine: String): Boolean = {
    val charFreq = magazine.foldLeft(Map[Char, Int]()) {
      case (acc, present) =>
        acc.updated(present, acc.getOrElse(present, 0) + 1)
    }
    val updatedCharFreq = ransomNote.foldLeft(charFreq) {
      case (acc, present) =>
        acc.updated(present, acc.getOrElse(present, 0) - 1)
    }
    updatedCharFreq.forall(_._2 >= 0)
  }

}
