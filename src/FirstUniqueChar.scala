import scala.annotation.tailrec

object FirstUniqueChar {
  def main(args: Array[String]): Unit = {
    assert(firstUniqChar(s = "loveleetcode") == 2)
    assert(firstUniqChar(s = "leetcode") == 0)
  }

  def firstUniqChar(s: String): Int = {
    val charFreq = s.foldLeft(Map[Char, Int]()) {
      case (acc, present) =>
        acc.updated(present, acc.getOrElse(present, 0) + 1)
    }

    @tailrec
    def _recursive(idx: Int = 0): Int = {
      if (idx < s.length) {
        if (charFreq(s(idx)) == 1) {
          idx
        } else {
          _recursive(idx + 1)
        }
      } else {
        -1
      }
    }

    _recursive()
  }

}
