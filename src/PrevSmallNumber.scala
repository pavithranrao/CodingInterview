import scala.collection.mutable

object PrevSmallNumber {

  def getPrevSmallNumber(array: Array[Int]): Array[Int] = {
    val stack = mutable.Stack[Int](array.head)

    array.tail.foldLeft(Array(-1)) {
      (acc, present) =>
        while (stack.nonEmpty && present.compare(stack.head) <= 0) {
          stack.pop()
        }

        val element = if (stack.nonEmpty) stack.head else -1
        stack.push(present)
        acc :+ element
    }

  }

  def main(args: Array[String]): Unit = {

    val array = Array(2, 5, 8, 9, 3, 7, 1, 4, 10, 6)
    println(s"The given array is : ${array.mkString(", ")}")

    val answer = getPrevSmallNumber(array)
    println(s"The first previous small number is : ${answer.mkString(", ")}")

  }

}
