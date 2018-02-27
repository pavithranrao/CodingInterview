import scala.annotation.tailrec

// Courtesy : https://www.youtube.com/watch?v=8LusJS5-AGo
object DiscreteKnapSack {

  type Item = (Int, Int)

  // Bottom Up Approach
  def getKnapSack(items: Array[Item], capacity: Int): (Int, Array[Item]) = {

    val m = items.length - 1
    val n = capacity
    val C = Array.ofDim[Int](m + 1, n + 1)

    for (row <- 1 to m) {
      for (col <- 1 to n) {
        C(row)(col) =
          if (items(row)._1 <= col) {
            items(row)._2 + C(row - 1)(col - items(row)._1) max C(row - 1)(col)
          } else {
            C(row - 1)(col)
          }
      }
    }

    @tailrec
    def getTrace(row: Int = m, col: Int = n,
                 acc: Array[Item] = Array[Item]()): Array[Item] = {
      val present = C(row)(col)
      if (present == 0) {
        acc
      } else {
        if (present == C(row - 1)(col)) {
          getTrace(row - 1, col, acc)
        } else if (present == C(row)(col - 1)) {
          getTrace(row, col - 1, acc)
        } else {
          getTrace(row - 1, col - items(row)._1, items(row) +: acc)
        }
      }
    }

    (C(m)(n), getTrace())
  }

  def main(args: Array[String]): Unit = {

    val items: Array[Item] = Array((1, 1), (3, 4), (4, 5), (5, 7))
    val capacity = 7

    val (maxValue, selectedItems) = getKnapSack((0, 0) +: items, capacity)
    println(s"The answer is $maxValue")
    println(s"The selected items are: ${selectedItems.mkString(", ")}")

  }
}

//
//Capacity ->	0	1	2	3	4	5	6	7
//Items       0	0	0	0	0	0	0	0
//(1, 1)	    0	1	1	1	1	1	1	1
//(3, 4)	    0	1	1	4	5	5	5	5
//(4, 5)	    0	1	1	4	5	6	6	9
//(5, 7)	    0	1	1	4	5	7	8	9
//
