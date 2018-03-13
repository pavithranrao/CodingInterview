import scala.annotation.tailrec

object ListOperations {

  def main(args: Array[String]): Unit = {

    val x = (1 to 10).toList

    val reversed = x.foldLeft(List[Int]()) {
      (accumulator, present) =>
        present +: accumulator
    }
    println(reverseList(x))
    println(reverse(x))
    println(reversed)

    printReverse(x)

    println()
    val (left, right) = getAlternatingElements(x)
    println(s"The left is ${left.mkString(", ")} and the right is ${right.mkString(", ")}")

    val alternateElements = removeAlternateElement(x)
    println(s"The alternate elements are ${alternateElements.mkString(", ")}")
  }

  def reverseList[A](list: List[A]): List[A] = list match {
    case h :: tail => reverseList(tail) ::: List(h)
    case Nil => Nil
  }

  def reverse[A](list: List[A]): List[A] = {
    @tailrec
    def _reverse(accumulator: List[A], remaining: List[A]): List[A] = remaining match {
      case Nil => accumulator
      case head :: tail => _reverse(head :: accumulator, tail)
    }

    _reverse(Nil, list)
  }

  def printReverse[A](list: List[A]): Unit = {
    def _reverse(remaining: List[A]): Unit = remaining match {
      case Nil => // Do nothing
      case head :: Nil => print(s"$head -> ")
      case head :: tail =>
        _reverse(tail)
        print(s"$head -> ")
    }

    _reverse(list)
  }


  def getAlternatingElements[A](sequence: Seq[A]): (Seq[A], Seq[A]) = {
    @tailrec
    def alternateList(in: Seq[A],
                      left: Seq[A] = Seq[A](),
                      right: Seq[A] = Seq[A]()): (Seq[A], Seq[A]) = {
      in match {
        case leftVal :: rightVal :: tail => alternateList(tail, left :+ leftVal, right :+ rightVal)
        case leftVal :: Nil => alternateList(Nil, left :+ leftVal, right)
        case _ => (left, right)
      }
    }

    alternateList(sequence)
  }

  def removeAlternateElement[A](sequence: Seq[A]): Seq[A] = {
    @tailrec
    def alternateList(list: Seq[A],
                      acc: Seq[A] = List[A]()): Seq[A] = {
      list match {
        case pick :: _ :: tail => alternateList(tail, acc :+ pick)
        case pick :: Nil => alternateList(Nil, acc :+ pick)
        case _ => acc
      }
    }

    alternateList(sequence)
  }

}
