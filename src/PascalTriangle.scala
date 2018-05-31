import scala.annotation.tailrec

object PascalTriangle {

  def main(args: Array[String]): Unit = {
    val answer = generate(5)
    println(answer.map(_.mkString(", ")).mkString("\n"))
  }

  def generate(numRows: Int): List[List[Int]] = {

    val init = List(List(1), List(1, 1))

    @tailrec
    def helper(n: Int = 3,
               list: List[List[Int]]): List[List[Int]] = {
      if (n > numRows) {
        list
      } else {
        val lastRow = list.last
        val lastLength = lastRow.length
        val newRow = 1 +: (0 until lastLength - 1).toList.map(idx => lastRow(idx) + lastRow(idx + 1)) :+ 1
        //  val newRow = 1 +: list.last.sliding(2).map(_.sum).toList :+ 1
        helper(n + 1, list :+ newRow)
      }
    }

    if (numRows > 2) {
      helper(list = init)
    } else {
      init.slice(0, numRows)
    }

  }

}
