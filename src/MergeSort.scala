object MergeSort {
  // Courtesy : https://gist.github.com/fancellu/5e00336840098c194551
  def main(args: Array[String]): Unit = {
    val list = List(1, 4, 6, 2, 7)
    println(s"Before Sort : ${list.mkString(", ")}")

    //    val answer = mergeSort(list)
    val comparatorFn = (a: Int, b: Int) => a < b
    val answer = genericMergeSort(list, comparatorFn)
    println(s"After Sort : ${answer.mkString(", ")}")

    assert(answer == list.sorted)
    assert(Nil == List.empty)
  }

  //  def mergeSort(list: List[Int]): List[Int] = {
  //
  //    def merge(left: List[Int], right: List[Int]): List[Int] = {
  //      (left, right) match {
  //        case (_, Nil) => left
  //
  //        case (Nil, _) => right
  //
  //        case (leftHead :: leftTail, rightHead :: rightTail) =>
  //          if (leftHead < rightHead) {
  //            leftHead :: merge(leftTail, right)
  //          }
  //          else {
  //            rightHead :: merge(left, rightTail)
  //          }
  //      }
  //    }
  //
  //    val n = list.length / 2
  //    if (n == 0) {
  //      list
  //    }
  //    else {
  //      val (left, right) = list.splitAt(n)
  //      merge(mergeSort(left), mergeSort(right))
  //    }
  //  }


  def genericMergeSort[A](list: List[A], comparatorFn: (A, A) => Boolean): List[A] = {

    def genericMerge(left: List[A], right: List[A]): List[A] = {
      (left, right) match {
        case (_, Nil) => left
        case (Nil, _) => right
        case (leftHead :: leftTail, rightHead :: rightTail) =>
          if (comparatorFn(leftHead, rightHead)) {
            leftHead :: genericMerge(leftTail, right)
          }
          else {
            rightHead :: genericMerge(left, rightTail)
          }
      }
    }

    val n = list.length / 2
    if (n == 0) {
      list
    }
    else {
      val (left, right) = list.splitAt(n)
      genericMerge(genericMergeSort(left, comparatorFn),
        genericMergeSort(right, comparatorFn))
    }
  }

}
