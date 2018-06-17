import scala.annotation.tailrec

object MoveZeroes {

  def main(args: Array[String]): Unit = {
    val input = Array(0, 0, 1, 3, 0, 2)
    moveZeroes(input)

    assert(input sameElements Array(1, 3, 2, 0, 0, 0))
  }

  // beats 94%
  def moveZeroes(nums: Array[Int]): Unit = {
    val len = nums.length

    @inline
    def swap(source: Int, dest: Int): Unit = {
      val temp = nums(source)
      nums(source) = nums(dest)
      nums(dest) = temp
    }

    @tailrec
    def helper(current: Int = 0)
              (implicit lastNonZeroFoundAt: Int = 0): Unit = {
      if (current < len) {
        if (nums(current) != 0) {
          swap(lastNonZeroFoundAt, current)
          helper(current + 1)(lastNonZeroFoundAt + 1)
        } else {
          helper(current + 1)
        }
      }
    }

    helper()

  }

}
