object LetterCasePermutation {

  def main(args: Array[String]): Unit = {
    val input = "a1B2"
    val answer = letterCasePermutation(input)

    assert(answer sameElements Array("a1b2", "a1B2", "A1b2", "A1B2"))
  }

  def letterCasePermutation(S: String): List[String] = {
    val len = S.length

    def dfs(idx: Int = 0,
            buffer: String = "",
            acc: List[String] = List()): List[String] = {
      if (idx == len) {
        acc :+ buffer
      } else {
        val chr = S(idx)
        if (chr.isLetter) {
          dfs(idx + 1, buffer + chr.toLower, acc) ++ dfs(idx + 1, buffer + chr.toUpper, acc)
        } else {
          dfs(idx + 1, buffer + chr, acc)
        }
      }
    }

    dfs()
  }

}
