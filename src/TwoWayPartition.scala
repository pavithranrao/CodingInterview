import Util.swap

import scala.annotation.tailrec

object TwoWayPartition {

  def twoWayPartition[A](array: Array[A], partitionFn: (A) => Boolean): Unit = {
    @tailrec
    def _recursive(left: Int = 0,
                   right: Int = array.length - 1): Unit = {
      if (left < right) {
        val newLeft = array.indexWhere(!partitionFn(_))
        val newRight = array.lastIndexWhere(partitionFn)
        if (newLeft < newRight) {
          swap(array, newLeft, newRight)
          _recursive(newLeft, newRight)
        }
      }
    }

    _recursive()
  }

  def main(args: Array[String]): Unit = {
    val array = Array(1, 11, 2, 12, 3, 13, 4, 14, 5, 15)
    println(s"The given array is : ${array.mkString(", ")}")

    //     val pivot = 10
    //     val partitionFn = (a: Int) => a < pivot

    //     Partition array into even and odd elements
    val partitionFn = (a: Int) => a % 2 == 0

    twoWayPartition(array, partitionFn)
    println(s"The two way partitioned array is : ${array.mkString(", ")}")
  }
}
