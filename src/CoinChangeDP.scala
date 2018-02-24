import scala.annotation.tailrec

// Courtesy : https://www.youtube.com/watch?v=NJuKJ8sasGk
object CoinChangeDP {

  def getMinChange(coins: Array[Int], target: Int): Array[Int] = {

    val C = 0 +: Array.fill(target)(Int.MaxValue - 1)
    val S = Array.fill(target + 1)(-1)

    for (coinIdx <- coins.indices) {
      val presentCoin = coins(coinIdx)
      for (changeIdx <- presentCoin to target) {
        val newChange = 1 + C(changeIdx - presentCoin)
        if (newChange < C(changeIdx)) {
          C(changeIdx) = newChange
          S(changeIdx) = coinIdx
        }
      }
    }
    // println(s"C => ${C.mkString(", ")}")
    // println(s"S => ${S.mkString(", ")}")

    @tailrec
    def _recursive(newTarget: Int, answer: Array[Int]): Array[Int] = {
      val coinIdx = S(newTarget)
      if (coinIdx == -1) {
        answer
      } else {
        val selected = coins(coinIdx)
        _recursive(newTarget - selected, answer :+ selected)
      }
    }

    _recursive(target, Array[Int]())
  }

  def main(args: Array[String]): Unit = {

    val coins = Array(7, 2, 3, 6)
    val target = 13

    val changes = getMinChange(coins, target)
    println(s"The minimum no of coins required is : ${changes.length}")
    println(s"The coins required are : ${changes.mkString(", ")}")

  }

}
