import scala.annotation.tailrec
import Util.swap

object ThreeWayPartition {

  def dutchPartition[A](array: Array[A], partitionFn: (A) => Int): Unit = {
    // Generic dutchPartition that accepts any object
    // and partitions the Array into three.

    @tailrec
    def _recursive(low: Int = 0, mid: Int = 0,
                   high: Int = array.length - 1): Unit = {
      if (mid <= high) {
        partitionFn(array(mid)) match {
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
    val array = Array(1, 10, 2, 4, 27, 15, 30, 5, 18)
    val pivot = (10, 20)
    println(s"The given array is ${array.mkString(", ")}")

    val partitionFn = (a: Int) => {
      (a.compare(pivot._1), a.compare(pivot._2)) match {
        case (-1, _) => 0
        case (1 | 0, -1 | 0) => 1
        case _ => 2
      }
    }

    dutchPartition(array, partitionFn)
    println(s"The 3 way partitioned array is ${array.mkString(", ")}")

  }
}
