object CamelCase {

  def camelcase(s: String): Int = {
    // Complete this function
    s.count(_.isUpper) + 1
  }

  def main(args: Array[String]) {
    val input = "saveChangesInTheEditor"
    val result = camelcase(input)
    println(s"The number of words in the $input are $result")
  }

}
