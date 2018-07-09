object CardWar {

  def main(args: Array[String]): Unit = {

  }

  def solution(a: String, b: String): Int = {
    // write your code in Scala 2.12
    val points = Map('T' -> 10,
      'J' -> 11,
      'Q' -> 12,
      'K' -> 13,
      'A' -> 14).withDefault((_: Char).asDigit)

    val (p1, p2) = a.zip(b).foldLeft((0, 0)) {
      case ((player1, player2), (move1, move2)) =>
        points(move1).compare(points(move2)) match {
          case 1 => (player1 + 1, player2)
          case -1 => (player1, player2 + 1)
          case _ => (player1, player2)
        }

    }

    p1 max p2
  }

}
