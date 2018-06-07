import scala.annotation.tailrec

object FirstPivot {

  def main(args: Array[String]): Unit = {
    val nums = Array(1, 7, 3, 6, 5, 6)
    val answer = pivotIndex(nums)

    assert(answer == 3)
  }

  def pivotIndex(nums: Array[Int]): Int = {

    val sum = nums.sum
    val len = nums.length

    @tailrec
    def getPivot(idx: Int = 0,
                 sumUpToHere: Int = 0): Int = {
      if (idx == len) {
        -1
      } else {
        if (sumUpToHere == sum - nums(idx) - sumUpToHere) {
          idx
        } else {
          getPivot(idx + 1, sumUpToHere + nums(idx))
        }
      }
    }

    getPivot()

  }

}
