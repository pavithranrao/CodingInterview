object ExcelNumToCol {

  def convertToTitle(n: Int): String = {
    @scala.annotation.tailrec
    def _recursive(in: Int = n, acc: String = ""): String = {
      if (in == 0) {
        acc
      } else {
        val nextChar: Char = ('A' + ((in - 1) % 26)).toChar
        _recursive((in - 1) / 26, nextChar +: acc)
      }
    }

    _recursive()
  }


  def main(args: Array[String]): Unit = {
    val input = 26
    val answer = convertToTitle(input)
    println(answer)
  }

}
