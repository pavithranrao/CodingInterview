import scala.annotation.tailrec

object OpenTheLock {
  def main(args: Array[String]): Unit = {
    val deadends = Array("8887", "8889", "8878", "8898", "8788", "8988", "7888", "9888")
    val target = "8888"
    val answer = openLock(deadends, target)
    println(answer)

    val deadends2 = Array("0201", "0101", "0102", "1212", "2002")
    val target2 = "0202"
    val answer2 = openLock(deadends2, target2)
    println(answer2)

    val deadends3 = Array("0000")
    val target3 = "8888"
    val answer3 = openLock(deadends3, target3)
    println(answer3)

  }


  def openLock(deadends: Array[String], target: String): Int = {
    val start = "0000"
    if (deadends.contains(start)) {
      -1
    } else {
      val next = Map(
        '0' -> List('9', '1'),
        '1' -> List('0', '2'),
        '2' -> List('1', '3'),
        '3' -> List('2', '4'),
        '4' -> List('3', '5'),
        '5' -> List('4', '6'),
        '6' -> List('5', '7'),
        '7' -> List('6', '8'),
        '8' -> List('7', '9'),
        '9' -> List('8', '0'))

      def getChildren(vertex: String): List[String] = {
        vertex.indices.foldLeft(List[String]()) {
          case (acc, index) =>
            val upDown = next(vertex(index))
            val newChildren = upDown.map {
              replace =>
                val (first, last) = vertex.splitAt(index)
                s"$first$replace${last.tail}"
            }
            acc ++ newChildren
        }
      }

      @tailrec
      def _bfs(vertices: Array[String],
               count: Int = 0,
               visitedSet: Set[String] = deadends.toSet): Int = {
        val fringe = vertices
          .flatMap(getChildren).distinct
          .filterNot { child => visitedSet.contains(child) }

        if (fringe.isEmpty) {
          -1
        } else {
          if (fringe.contains(target)) {
            count + 1
          } else {
            _bfs(fringe, count + 1, visitedSet ++ fringe)
          }
        }
      }

      _bfs(Array(start))
    }
  }

}
