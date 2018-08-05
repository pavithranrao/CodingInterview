import scala.annotation.tailrec

object Solution {
  def strStr(haystack: String, needle: String): Int = {

    val needleLen = needle.length
    val hayLen = haystack.length

    @tailrec
    def findNeedle(hayIdx: Int = 0,
                   needleIdx: Int = 0,
                   start: Int = 0): Int = {
      if (needleIdx == needleLen) {
        start
      } else {
        if (hayIdx == hayLen) {
          -1
        } else {
          if (haystack(hayIdx) == needle(needleIdx)) {
            if (needleIdx == 0) {
              findNeedle(hayIdx + 1, needleIdx + 1, hayIdx)
            } else {
              findNeedle(hayIdx + 1, needleIdx + 1, start)
            }
          } else {
            findNeedle(start + 1, 0, start + 1)
          }
        }
      }
    }

    if(needleLen > 0)
        findNeedle()
    else
      0
  }
}
