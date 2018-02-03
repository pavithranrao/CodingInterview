object Util {

  def invertMap[A, B](inputMap: Map[A, B]): Map[B, List[A]] = {
    inputMap.foldLeft(Map[B, List[A]]()) {
      case (mapAccumulator, currentElement) =>
        if (mapAccumulator.contains(currentElement._2)) {
          val newValue = mapAccumulator(currentElement._2) :+ currentElement._1
          mapAccumulator - currentElement._2 + (currentElement._2 -> newValue)
        } else {
          mapAccumulator + (currentElement._2 -> List(currentElement._1))
        }
    }
  }
}
