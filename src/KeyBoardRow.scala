object KeyBoardRow {

  def findWords(words: Array[String]): Array[String] = {
    val keyMap = Map(1 -> "qwertyuiopQWERTYUIOP", 2 -> "asdfghjklASDFGHJKL", 3 -> "zxcvbnmZXCVBNM")
    val keyMapper = keyMap.foldLeft(Map[Char, Int]()) {
      case (acc, (key, value)) =>
        value.foldLeft(acc) {
          case (innerAcc, present) =>
            innerAcc.updated(present, key)
        }
    }
    words.filter {
      present =>
        val headKey = keyMapper(present.head)
        present.tail.forall(x => keyMapper(x) == headKey)
    }
  }

  def main(args: Array[String]): Unit = {
    val words = Array("Hello", "Alaska", "Dad", "Peace")
    println(s"The input words are ${words.mkString(", ")}")

    val answer = findWords(words)
    println(s"The words with one row keys are : ${answer.mkString(", ")}")
  }

}
