object JudgeCircle {
  def judgeCircle(moves: String): Boolean = {
    val (effHor, effVer) =
      moves.foldLeft((0, 0)) {
        case ((accVer, accHor), move) =>
          move match {
            case 'U' => (accVer + 1, accHor)
            case 'D' => (accVer - 1, accHor)
            case 'L' => (accVer, accHor + 1)
            case 'R' => (accVer, accHor - 1)
          }
      }
    effHor == 0 && effVer == 0
  }

  def main(args: Array[String]): Unit = {
    val moves = "RLUURDDDLU"
    val answer = judgeCircle(moves)

    if (answer)
      println(s"The moves form a circle ")
    else
      println(s"The moves doesn't form a circle ")
  }

}
