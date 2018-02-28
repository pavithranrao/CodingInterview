import Util.Matrix
import scala.annotation.tailrec

// Courtesy : https://www.youtube.com/watch?v=NnD96abizww
object LongestCommonSubSequence {

  def getLongestCommonSubSequence(input1: String, input2: String): (Int, Array[Char]) = {

    val m = input1.length
    val n = input2.length

    val C: Matrix[Int] = Array.ofDim[Int](m + 1, n + 1)
    // Array.ofDim[Int](m + 1, n + 1) <=> Array.fill(m + 1)(Array.fill(n + 1)(0))
    // println(s"C dimensions : ${C.length} x ${C.head.length}")

    for (row <- 1 to m) {
      for (col <- 1 to n) {
        C(row)(col) =
          if (input1(row - 1) == input2(col - 1)) {
            1 + C(row - 1)(col - 1)
          } else {
            C(row - 1)(col) max C(row)(col - 1)
          }
      }
    }
    // Util.printMatrix(C)

    @tailrec
    def getTrace(row: Int = m, col: Int = n,
                   acc: Array[Char] = Array[Char]()): Array[Char] = {
      val present = C(row)(col)
      if (present == 0) {
        acc
      } else {
        if (present == C(row - 1)(col)) {
          getTrace(row - 1, col, acc)
        } else if (present == C(row)(col - 1)) {
          getTrace(row, col - 1, acc)
        } else if (present == 1 + C(row - 1)(col - 1)) {
          getTrace(row - 1, col - 1, input1(row - 1) +: acc)
        } else {
          acc
        }
      }
    }

    (C(m)(n), getTrace())
  }

  def main(args: Array[String]): Unit = {

    val input1 = "ALGORITHM"
    val input2 = "ALTRUISTIC"
    println(s"The given inputs are $input1 and $input2")

    val (length, subSequence) = getLongestCommonSubSequence(input1, input2)
    println(s"The maximum length possible is $length")
    println(s"The longest common sub sequence is : ${subSequence.mkString("")}")

  }

}

//
//          0 1	2	3	4	5	6	7	8	9	10
//          _ A	L	T	R	U	I	S	T	I	C
//      0 _ 0	0	0	0	0	0	0	0	0	0	0
//      1	A	0	1	1	1	1	1	1	1	1	1	1
//      2	L	0	1	2	2	2	2	2	2	2	2	2
//      3	G	0	1	2	2	2	2	2	2	2	2	2
//      4	O	0	1	2	2	2	2	2	2	2	2	2
//      5	R	0	1	2	2	3	3	3	3	3	3	3
//      6	I	0	1	2	2	3	3	4	4	4	4	4
//      7	T	0	1	2	3	3	3	4	4	5	5	5
//      8	H	0	1	2	3	3	3	4	4	5	5	5
//      9	M	0	1	2	3	3	3	4	4	5	5	5
//