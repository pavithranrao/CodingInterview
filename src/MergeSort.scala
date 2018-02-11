object MergeSort {
  // Courtesy : https://gist.github.com/fancellu/5e00336840098c194551
  def main(args: Array[String]): Unit = {
    val list = List(1, 4, 6, 2, 7)
    println(s"Before Sort : ${list.mkString(", ")}")

    val comparatorFn = (a: Int, b: Int) => a < b
    val answer = genericMergeSort(list)(comparatorFn)
    println(s"After Sort : ${answer.mkString(", ")}")

    assert(answer == list.sorted)
    assert(Nil == List.empty)
  }

  def genericMergeSort[A](list: List[A])
                         (implicit comparatorFn: (A, A) => Boolean): List[A] = {

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

    val mid = list.length / 2
    if (mid == 0) {
      list
    }
    else {
      val (left, right) = list.splitAt(mid)
      genericMerge(genericMergeSort(left), genericMergeSort(right))
    }
  }

}
