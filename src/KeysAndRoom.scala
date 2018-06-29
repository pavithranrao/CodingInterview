object KeysAndRoom {

  def main(args: Array[String]): Unit = {
    val rooms = List(List(1), List(2), List(3), List[Int]())
    assert(canVisitAllRooms(rooms))

    val negRooms = List(List(1, 3), List(3, 0, 1), List(2), List(0))
    assert(!canVisitAllRooms(negRooms))

  }

  // accepted
  def canVisitAllRooms(rooms: List[List[Int]]): Boolean = {

    def dfs(visitedSet: Set[Int] = Set[Int](),
            vertex: Int = 0): Set[Int] = {
      if (visitedSet.contains(vertex)) {
        visitedSet
      } else {
        //  val children = rooms(vertex)
        //  children.foldLeft(visitedSet + vertex) {
        //    case (accSet, child) =>
        //      dfs(accSet, child)
        //  }
        rooms(vertex).foldLeft(visitedSet + vertex)(dfs)
      }
    }

    val visited = dfs()
    visited.size == rooms.length

  }

}
