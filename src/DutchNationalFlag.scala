import Util.swap
import scala.annotation.tailrec

object DutchNationalFlag {

  def partitionSwap(array: Array[Int]): Unit = {
    // Refer ThreeWayPartition for generic partitioning

    @tailrec
    def _recursive(low: Int = 0, mid: Int = 0,
                   high: Int = array.length - 1): Unit = {
      if (mid <= high) {
        array(mid) match {
          case 0 =>
            swap(array, low, mid)
            _recursive(low + 1, mid + 1, high)
          case 1 =>
            _recursive(low, mid + 1, high)
          case 2 =>
            swap(array, high, mid)
            _recursive(low, mid, high - 1)
        }
      }
    }

    _recursive()
  }

  def main(args: Array[String]): Unit = {

    val array = Array(0, 0, 0, 1, 1, 2, 1, 1, 0, 1, 1, 0, 0, 2, 1, 1)
    println(s"Before array segregation : ${array.mkString(", ")}")

    partitionSwap(array)
    println(s"After array segregation : ${array.mkString(", ")}")

    val partitionFn = (a: Int) => a
    ThreeWayPartition.dutchPartition(array, partitionFn)
    println(s"Answer from Three way partitioning ${array.mkString(", ")}")
  }
}
