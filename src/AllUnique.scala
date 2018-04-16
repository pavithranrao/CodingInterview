object AllUnique {

  def allUnique(input: Array[Int]): Boolean = {
    input.length > 0 && input.toSet.size == input.length
  }

  def main(args: Array[String]): Unit = {
    val input = Array(1, 2, 6, 3, 9)
    val answer = allUnique(input)
    println(answer)
    
    assert(!allUnique(Array()))
  }


}
