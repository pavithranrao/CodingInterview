import scala.annotation.tailrec

object SmallestMissingNumber {

  @tailrec
  def findFirstMissing(start: Int = 0, end: Int)
                      (implicit array: Array[Int], offset: Int = 0): Int = {
    // offset is made implicit as it is a constant for a given array
    // array is made implicit to avoid passing it again and again
    // println(s"${array.mkString(", ")} $start, $end, $offset")
    if (start > end) {
      end + 1
    } else if (array(start) != start + offset) {
      start + offset
    } else {
      val mid = (start + end) / 2
      if (array(mid) == mid + offset) {
        findFirstMissing(mid + 1, end)
      } else {
        findFirstMissing(start, mid)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    // val array = Array(0, 1, 2, 3, 4, 5, 6, 7, 10)
    val array = Array(6, 7, 8, 10)

    // offset is the first element in the array
    // offset is 0 + the first element in the positive integers array
    val missingNumber = findFirstMissing(0, array.length - 1)(array, array.head)
    println(s"The first missing number in ${array.mkString(", ")} is $missingNumber")

    assert(missingNumber == 9)

  }

}
