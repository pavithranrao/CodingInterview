object LCIS {

  def main(args: Array[String]): Unit = {
    val input = Array(1, 3, 5, 4, 2, 3, 4, 5)
    val answer = findLengthOfLCIS(input)
    println(answer)
  }

  // modified Kadanes' Algorithm
  def findLengthOfLCIS(array: Array[Int]): Int = {
    if (array.length > 1) {
      array.tail.foldLeft((1, 1, array.head)) {
        case ((maxUpToHere, maxSoFar, prev), present) =>
          // if present is greater than prev
          //    maxEndingHere is increased by 1
          // else
          //    maxEndingHere is reset to 1
          val maxEndingHere =
            if (present > prev) {
              maxUpToHere + 1
            } else {
              1
            }
          // maxSoFar is the max of maxEndingHere and maxSoFar
          (maxEndingHere, maxEndingHere max maxSoFar, present)
      }._2
    } else {
      array.length
    }
  }

  // slower implementation
  // bitonic sequence approach
  def findLengthOfLCIS2(array: Array[Int]): Int = {
    if (array.length > 1) {
      val inc =
        array.tail.
          foldLeft((Array(1), array.head)) {
            case ((acc, prev), present) =>
              if (present > prev) {
                (acc :+ (acc.last + 1), present)
              } else {
                (acc :+ 1, present)
              }
          }._1

      val dec =
        array.slice(0, array.length - 1).
          foldRight(Array(1), array.last) {
            case (present, (acc, prev)) =>
              if (present < prev) {
                (acc :+ (acc.last + 1), present)
              } else {
                (acc :+ 1, present)
              }

          }._1.reverse

      //  println(inc.mkString(", "))
      //  println(dec.mkString(", "))
      inc.zip(dec).map(x => x._1 + x._2).max
    } else {
      array.length
    }
  }

}
