object CompositeFuncs {

  def inputStringPipeline(inputStr: String): String => String = {
    val upperCase = (input: String) => input.toUpperCase
    val lowerCase = (input: String) => input.toLowerCase
    val titleCase = (input: String) => input.split(' ').map(_.capitalize).mkString(" ")
    val reverse = (input: String) => input.reverse
    val sortChars = (input: String) => input.sorted
    val replaceChar = (input: String) => input.replaceAll("\\s", "")
    val emptyFunc: String => String = (input: String) => input
    
    inputStr.foldLeft(emptyFunc) {
      case (funcAcc, present) =>
        funcAcc andThen (present.toString match {
          case "U" => upperCase
          case "l" => lowerCase
          case "T" => titleCase
          case "r" => reverse
          case "s" => sortChars
          case "*" => replaceChar
        })
    }

  }

  def main(args: Array[String]): Unit = {
    val input = "Us*"
    val func = inputStringPipeline(input)
    val str = func("age bfh dc")
    println(str)

  }

}
