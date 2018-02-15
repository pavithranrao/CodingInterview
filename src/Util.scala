object Util {

  case class Node(nodeValue: Int) {
    val value: Int = nodeValue
    var next: Option[Node] = None
  }

  def invertMap[A, B](inputMap: Map[A, B]): Map[B, List[A]] = {
    inputMap.foldLeft(Map[B, List[A]]()) {
      case (mapAccumulator, (value, key)) =>
        if (mapAccumulator.contains(key)) {
          mapAccumulator.updated(key, mapAccumulator(key) :+ value)
        } else {
          mapAccumulator.updated(key, List(value))
        }
    }
  }
}
