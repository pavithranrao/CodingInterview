object SecondLargest {

  def getSecondLargest(array: Array[Int]): Either[Int, _] = {
    if (array.length <= 1) {
      Right(None)
    } else {
      val answer = array.foldLeft((Int.MinValue, Int.MinValue)) {
        case ((first, second), present) =>
          (present.compare(first), present.compare(second)) match {
            case (1, _) => (present, first)
            case (-1, 1) => (first, present)
            case _ => (first, second)
          }
      }._2
      Left(answer)
    }
  }

  def main(args: Array[String]): Unit = {
    val array = Array(12, 35, 1, 10, 34, 1, 35)
    println(s"The given array is : ${array.mkString(", ")}")

    val answer = getSecondLargest(array)
    answer match {
      case Left(secondMax) => println(s"The second largest number is : $secondMax")
      case Right(_) => println(s"The array is invalid")
    }
  }

}
