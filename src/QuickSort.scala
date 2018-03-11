import scala.reflect.ClassTag

object QuickSort {


  def quickSort[A: ClassTag](array: Array[A], comparatorFn: (A, A) => Boolean): Array[A] = {
    if (array.length <= 1) {
      array
    }
    else {
      val pivot = array(array.length / 2)
      val (left, right) = array.partition(comparatorFn(_, pivot))
      quickSort(left, comparatorFn) ++ quickSort(right, comparatorFn)
    }
  }

  def genericQuickSort[A](start: Int = 0, end: Int)
                         (implicit array: Array[A],
                          comparatorFn: (A, A) => Boolean = (a: Int, b: Int) => a < b): Array[A] = {

    if (start < end) {
      val partitionIndex = genericPartition(start, end)

      genericQuickSort(start, partitionIndex - 1)
      genericQuickSort(partitionIndex + 1, end)
    }
    array
  }

  def genericPartition[A](start: Int, end: Int)
                         (implicit array: Array[A], comparatorFn: (A, A) => Boolean): Int = {

    val pivot = array(end)
    var wall = start

    def swap(source: Int, dest: Int) {
      val temp = array(source)
      array(source) = array(dest)
      array(dest) = temp
    }

    for (idx <- start until end) {
      if (comparatorFn(array(idx), pivot)) {
        swap(idx, wall)
        wall += 1
      }
    }
    swap(wall, end)

    wall
  }

  def main(args: Array[String]): Unit = {

    val array = Array(1, 4, 6, 2, 7)
    println(s"Before Sort : ${array.mkString(", ")}")

    val comparatorFn = (a: Int, b: Int) => a < b
    val answer = genericQuickSort(0, array.length - 1)(array, comparatorFn)
    println(s"After Sort : ${answer.mkString(", ")}")

    val stringArray = Array("abcd", "a", "ab", "abcde", "abc")
    println(s"Before Sort : ${stringArray.mkString(", ")}")

    val stringComparatorFn = (a: String, b: String) => a.length < b.length
    val stringAnswer = genericQuickSort(0, stringArray.length - 1)(stringArray, stringComparatorFn)
    println(s"After Sort : ${stringAnswer.mkString(", ")}")

    val fpArray = Array(1, 4, 6, 2, 7)
    val fpAnswer = quickSort(fpArray, comparatorFn)

    assert(answer sameElements fpAnswer)
  }

}
