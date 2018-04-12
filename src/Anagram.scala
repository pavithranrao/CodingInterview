object Anagram {

  def main(args: Array[String]): Unit = {
    val s = "anagram"
    val t = "nagaram"
    val answer = isAnagram(s, t)

    println(answer)
    assert(!isAnagram(s = "rat", t = "car"))
  }

  def isAnagram(s: String, t: String): Boolean = {
    if (s.length == t.length) {
      val charFreq = s.foldLeft(Map[Char, Int]()) {
        case (acc, present) =>
          acc.updated(present, acc.getOrElse(present, 0) + 1)
      }
      val updatedCharFreq = t.foldLeft(charFreq) {
        case (acc, present) =>
          acc.updated(present, acc.getOrElse(present, 0) - 1)
      }

      updatedCharFreq.forall(_._2 == 0)
    } else {
      false
    }
  }

}
