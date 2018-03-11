import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

object Util {

  // generate infinite cyclic stream of Seq[A]
  def cyclicIterator[A](xs: Seq[A]): Iterator[A] =
    Stream.continually(xs).flatten.iterator

  def memoize[I, O](f: I => O): I => O = new mutable.HashMap[I, O]() {self =>
    override def apply(key: I): O = self.synchronized(getOrElseUpdate(key, f(key)))
  }

  case class Node[A: ClassTag](value: A,
                               var next: Option[Node[A]] = None) {

    def push(element: Node[A]): Unit = {
      var current = this
      while (current.next.isDefined) {
        current = current.next.get
      }

      current.next = Some(element)
    }

    def printList(): Unit = {
      var current = Option(this)
      while (current.isDefined) {
        print(s" -> ${current.get.value}")
        current = current.get.next
      }
      println("\n")
    }


    def getMiddleElement: Option[Node[A]] = {
      var slowPtr = this
      var fastPtr = this.next.get

      while (fastPtr.next.isDefined && fastPtr.next.get.next.isDefined) {
        fastPtr = fastPtr.next.get.next.get
        slowPtr = slowPtr.next.get
      }

      Some(slowPtr)
    }

    def sort(head: Option[Node[A]])
            (implicit comparatorFn: (A, A) => Boolean): Option[Node[A]] = {
      MergeSortLinkedList.mergerSort(head)
    }

    def copyNode(): Node[A] = {
      this.copy(next = None)
    }

    def length(head: Node[A] = this): Int = {
      var length = 0
      var current: Option[Node[A]] = Some(head)
      while (current.isDefined) {
        length += 1
        current = current.get.next
      }
      length
    }

    // Courtesy : https://gist.github.com/pathikrit/4af1c1c2c590c2478b0f
    @tailrec
    final def reverse(present: Node[A] = this,
                      prev: Option[Node[A]] = None): Node[A] = {
      // Read : https://alvinalexander.com/scala/scala-case-class-copy-method-example
      val reversed = present.copy(next = prev)
      present.next match {
        case None => reversed
        case Some(nextNode) => reverse(nextNode, Some(reversed))
      }
    }
  }


  // generic TreeNode with A as value
  case class TreeNode[A: ClassTag](value: A,
                                   var left: Option[TreeNode[A]] = None,
                                   var right: Option[TreeNode[A]] = None)

  def swap[A](array: Array[A], source: Int, dest: Int): Unit = {
    val temp = array(source)
    array(source) = array(dest)
    array(dest) = temp
  }


  type Matrix[A] = Array[Array[A]]

  def printMatrix[A](matrix: Matrix[A]): Unit = {
    for (row <- matrix.indices) {
      for (col <- matrix.head.indices) {
        print(s"${matrix(row)(col)}\t")
      }
      println()
    }
  }

  def invertMap[A, B](inputMap: Map[A, B]): Map[B, Seq[A]] = {
    inputMap.foldLeft(Map[B, Seq[A]]()) {
      case (mapAcc, (value, key)) =>
        if (mapAcc.contains(key)) {
          mapAcc.updated(key, mapAcc(key) :+ value)
        } else {
          mapAcc.updated(key, Seq(value))
        }
    }
  }

  def shuffleArray[T](array: Array[T]): Unit = {
    val rnd = new scala.util.Random
    val length = array.length - 1
    for (idx <- array.indices) {
      val shuffleIdx = rnd.nextInt(length)
      swap(array, idx, shuffleIdx)
    }
  }

  def logNBaseB(n: Int, b: Int): Double = {
    import math.log
    log(n) / log(b)
  }
}
