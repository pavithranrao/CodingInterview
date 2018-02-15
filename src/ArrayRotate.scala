import MemoisationDemo.gcd
import scala.util.control.Breaks.{breakable, break}

// Courtesy : https://www.youtube.com/watch?v=utE_1ppU5DY

object ArrayRotate {

  def rotateArray[A](array: Array[A], rotate: Int, numSets: Int): Unit = {
    val n = array.length
    var d = -1
    var temp = array.head
    var j = 0
    for (i <- 1 to numSets) {
      j = i
      temp = array(i)
      breakable(
        while (true) {
          d = (j + rotate) % n
          if (d == i)
            break
          array(j) = array(d)
          j = d
          array(j) = temp
        })
    }
  }

  def main(args: Array[String]): Unit = {
    val array = (1 to 18).toArray
    val rotate = 12
    println(s"The given array is : ${array.mkString(", ")}")

    val memo = Memoization()
    lazy val memoGCD = memo.memoizeFn(gcd _ tupled)
    val numSets = memoGCD(array.length, rotate)

    rotateArray(array, rotate, numSets)
    println(s"The given array rotated by $rotate is : ${array.mkString(", ")}")
  }

}
