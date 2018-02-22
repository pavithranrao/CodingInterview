import scala.annotation.tailrec

object BinarySearch {

  def bst(input: Array[Int], target: Int): Option[Int] = {
    @tailrec
    def recursiveSearch(low: Int, high: Int): Option[Int] =
      (low + high) / 2 match {
        case _ if high < low => None
        case mid if input(mid) > target => recursiveSearch(low, mid - 1)
        case mid if input(mid) < target => recursiveSearch(mid + 1, high)
        case mid => Some(mid)
      }
    
    recursiveSearch(0, input.length - 1)
  }

  def main(args: Array[String]): Unit = {
    //    val array = Array(1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6)
    val array = (1 to 10).toArray
    println(s"Given array : ${array.zipWithIndex.mkString(", ")}")

    val target = 6

    val answer = bst(array, target)
    if (answer.isDefined) {
      println(s"The number $target is found at ${answer.get}")
    }
    else {
      println(s"The number $target is not found")
    }
  }

}


