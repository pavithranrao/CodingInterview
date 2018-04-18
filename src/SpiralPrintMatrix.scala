import scala.annotation.tailrec
import scala.reflect.ClassTag
//import scala.collection.mutable.ArrayBuffer

object SpiralPrintMatrix {

  def printMatrix[T](matrix: Array[Array[T]]): Unit = {
    println(matrix.map(_.mkString("\t")).mkString("\n"))
  }

  def spiralPrint[T: ClassTag](matrix: Array[Array[T]]): Array[T] = {
    def getUpdatedBuffer(runner: Range, fixed: Int, isHorizontal: Boolean): Array[T] = {
      runner.foldLeft(Array[T]()) {
        case (acc, idx) =>
          if (isHorizontal) {
            acc :+ matrix(fixed)(idx)
          }
          else {
            acc :+ matrix(idx)(fixed)
          }
      }
    }

    @tailrec
    def _recursive(top: Int = 0, bottom: Int = matrix.length - 1,
                   left: Int = 0, right: Int = matrix.head.length - 1,
                   direction: Int, buffer: Array[T]): Array[T] = {
      // direction = 0 implies →
      // direction = 1 implies ↓
      // direction = 2 implies ←
      // direction = 3 implies ↑

      // the direction is horizontal for 0 and 2
      // the direction is vertical   for 1 and 3
      val isHorizontal = direction % 2 == 0

      if (top <= bottom && left <= right) {
        direction match {
          case 0 =>
            val update = getUpdatedBuffer(left to right, top, isHorizontal)
            _recursive(top + 1, bottom, left, right, direction = 1, buffer ++ update)
          case 1 =>
            val update = getUpdatedBuffer(top to bottom, right, isHorizontal)
            _recursive(top, bottom, left, right - 1, direction = 2, buffer ++ update)
          case 2 =>
            val update = getUpdatedBuffer(right to left by -1, bottom, isHorizontal)
            _recursive(top, bottom - 1, left, right, direction = 3, buffer ++ update)
          case 3 =>
            val update = getUpdatedBuffer(bottom to top by -1, left, isHorizontal)
            _recursive(top, bottom, left + 1, right, direction = 0, buffer ++ update)
        }
      } else {
        buffer
      }
    }

    _recursive(direction = 0, buffer = Array[T]())
  }

  def main(args: Array[String]): Unit = {

    val matrix = Array(
      (1 to 4).toArray,
      (5 to 8).toArray,
      (9 to 12).toArray,
      (13 to 16).toArray)

    println(s"The given matrix is : ")
    printMatrix(matrix)

    val answer = spiralPrint(matrix)
    println(s"The matrix in spiral order is : ${answer.mkString(", ")}")

    //  Output:
    //
    //      >>> The given matrix is :
    //          1  2   3   4
    //          5  6   7   8
    //          9  10  11  12
    //          13 14  15  16
    //
    //
    //      >>> The matrix in spiral order is :
    //          1, 2, 3, 4, 8, 12, 16, 15, 14, 13, 9, 5, 6, 7, 11, 10
    //

  }
}

  //  def spiralPrint[T: ClassTag](matrix: Array[Array[T]]): Array[T] = {
  //
  //    var top = 0
  //    var bottom = matrix.length - 1
  //    var left = 0
  //    var right = matrix.head.length - 1
  //    var direction = 0
  //
  //    val arrayBuffer = ArrayBuffer[T]()
  //
  //    while (top <= bottom && left <= right) {
  //      direction match {
  //        case 0 =>
  //          for (idx <- left to right) {
  //            arrayBuffer += matrix(top)(idx)
  //          }
  //          direction = 1
  //          top += 1
  //        case 1 =>
  //          for (idx <- top to bottom) {
  //            arrayBuffer += matrix(idx)(right)
  //          }
  //          direction = 2
  //          right -= 1
  //        case 2 =>
  //          for (idx <- right to left by -1) {
  //            arrayBuffer += matrix(bottom)(idx)
  //          }
  //          direction = 3
  //          bottom -= 1
  //        case 3 =>
  //          for (idx <- bottom to top by -1) {
  //            arrayBuffer += matrix(idx)(left)
  //          }
  //          direction = 0
  //          left += 1
  //      }
  //    }
  //
  //    arrayBuffer.toArray
  //  }
