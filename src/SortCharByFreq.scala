//import scala.annotation.tailrec

object SortCharByFreq {

  def main(args: Array[String]): Unit = {
    val input = "tree"
    val answer = frequencySort(input)
    println(answer)

    assert(answer == "eetr" || answer == "eert")
  }

  def frequencySort(s: String): String = {
    val pq = scala.collection.mutable.PriorityQueue[(Int, Char)]()
    s.foldLeft(Map[Char, Int]().withDefaultValue(0)) {
      case (acc, present) =>
        acc.updated(present, acc(present) + 1)
    }.foreach {
      case (key, value) =>
        pq += ((value, key))
    }

    //    @tailrec
    //    def helper(acc: String = ""): String = {
    //      if (pq.isEmpty) {
    //        acc
    //      } else {
    //        val (count, char) = pq.dequeue
    //        helper(acc + char.toString * count)
    //      }
    //    }

    //    helper()
    pq.dequeueAll.foldLeft("") {
      case (acc, (count, char)) =>
        acc + char.toString * count
    }
  }

}
