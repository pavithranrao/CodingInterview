import scala.annotation.tailrec


// https://leetcode.com/problems/student-attendance-record-i/description/
object AttendanceCheck {

  def main(args: Array[String]): Unit = {
    val inputs = Array("PPALLP", "PPALLL", "LLL")
    val outputs = inputs.map(checkRecord)

    assert(outputs sameElements Array(true, false, false))
  }

  // beats 100%
  def checkRecord(s: String): Boolean = {
    val len = s.length

    @tailrec
    def check(idx: Int = 0,
              absentBefore: Boolean = false): Boolean = {
      // you survived!
      if (idx == len) {
        true
      } else {
        s(idx) match {
          case 'P' =>
            // present, move to next day
            check(idx + 1, absentBefore)
          case 'A' =>
            // if absent before, false
            if (absentBefore) {
              false
            } else {
              // else strike ONE
              check(idx + 1, absentBefore = true)
            }
          case _ =>
            // if late consecutively for 3 days, false
            if (idx >= 2 && s(idx - 1) == 'L' && s(idx - 2) == 'L') {
              false
            } else {
              // don't be late tomorrow
              check(idx + 1, absentBefore)
            }
        }
      }
    }

    check()
  }

}
