object NumberComplement {

  def findComplement(num: Int): Int = {
    val bin = num.toBinaryString.map { x => if (x == '1') '0' else '1' }
    Integer.parseInt(bin, 2)
  }

  def main(args: Array[String]): Unit = {
    val input = 5

    val answer = findComplement(input)
    println(s"The complement of $input is $answer")
  }

}
