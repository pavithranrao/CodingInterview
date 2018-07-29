object BinaryWatch {

  def main(args: Array[String]): Unit = {
    val answer = readBinaryWatch(num = 1)
    println(answer.mkString(", "))
  }

  def readBinaryWatch(num: Int): List[String] = {

    def dfs(n: Int, idx: Int,
            hours: Int, minutes: Int,
            acc: List[String]): List[String] = {
      if (hours >= 12 || minutes >= 60) {
        acc
      } else {
        if (n == 0) {
          acc :+ f"$hours:$minutes%02d"
        } else {
          (idx to 10).foldLeft(acc) {
            case (newAcc, i) =>
              if (i < 4) {
                dfs(n - 1, i + 1, hours + (1 << i), minutes, newAcc)
              } else {
                val k = i - 4
                dfs(n - 1, i + 1, hours, minutes + (1 << k), newAcc)
              }
          }
        }
      }
    }

    dfs(num, idx = 0, hours = 0, minutes = 0, List[String]())
  }


}
