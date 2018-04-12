import Util.swap

object WaveForm {

  def generateWave(input: Array[Int]): Unit = {
    val length = input.length
    for (idx <- input.indices by 2) {
      if (idx > 0 && input(idx - 1) > input(idx)) {
        swap(input, idx, idx - 1)
      }
      if (idx < length - 1 && input(idx + 1) > input(idx)) {
        swap(input, idx, idx + 1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val input = Array(10, 5, 6, 3, 2, 20, 100, 80)
    println(s"The given array is : ${input.mkString(", ")}")

    generateWave(input = input)
    println(s"The given array in wave form is : ${input.mkString(", ")}")
  }

}
