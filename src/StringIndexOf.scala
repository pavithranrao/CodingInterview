import scala.annotation.tailrec

object StringIndexOf {

  def main(args: Array[String]): Unit = {
    val haystack = "mississippi"
    val needle = "issipi"

    val answer = strStr(haystack, needle)
    assert(answer == -1)

    assert(strStr("mississippi", "issip") == 4)
  }

  // beats 100%
  def strStr(haystack: String, needle: String): Int = {

    val needleLen = needle.length
    val hayLen = haystack.length

    @tailrec
    def findNeedle(hayIdx: Int = 0,
                   needleIdx: Int = 0,
                   start: Int = 0): Int = {
      (needleIdx, hayIdx) match {
        case _ if needleIdx == needleLen =>
          start
        case _ if hayIdx == hayLen =>
          -1
        case _ if haystack(hayIdx) == needle(needleIdx) =>
          if (needleIdx == 0) {
            findNeedle(hayIdx + 1, needleIdx + 1, hayIdx)
          } else {
            findNeedle(hayIdx + 1, needleIdx + 1, start)
          }
        case _ => findNeedle(hayIdx = start + 1, start = start + 1)
      }

      //  if (needleIdx == needleLen) {
      //    start
      //  } else {
      //    if (hayIdx == hayLen) {
      //      -1
      //    } else {
      //      if (haystack(hayIdx) == needle(needleIdx)) {
      //        if (needleIdx == 0) {
      //          findNeedle(hayIdx + 1, needleIdx + 1, hayIdx)
      //        } else {
      //          findNeedle(hayIdx + 1, needleIdx + 1, start)
      //        }
      //      } else {
      //        findNeedle(hayIdx = start + 1, start = start + 1)
      //      }
      //    }
      //  }
    }

    if (needleLen > 0) {
      findNeedle()
    } else {
      0
    }
  }


}
