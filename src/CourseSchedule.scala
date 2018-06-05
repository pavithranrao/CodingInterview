object CourseSchedule {

  def main(args: Array[String]): Unit = {
    val numCourses = 3
    val prerequisites = Array(Array(0, 2), Array(1, 2), Array(2, 0))
    //  val prerequisites = Array(Array(1, 0), Array(1, 2), Array(0, 1))
    //  val numCourses = 2
    //  val prerequisites = Array(Array(1, 0))
    assert(!canFinish(numCourses, prerequisites))

    assert(canFinish(numCourses = 0, Array[Array[Int]]()))

  }

  def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {

    val init = Map[Int, List[Int]]().withDefaultValue(List[Int]())

    val adjList = prerequisites.foldLeft(init) {
      case (acc, Array(source, dest)) =>
        acc.updated(source, acc(source) :+ dest)
    }

    def dfs(vertex: Int,
            visitedSet: Set[Int] = Set[Int]()): Boolean = {
      // detects cycle
      if (visitedSet.contains(vertex)) {
        // if vertex is visited, return false
        false
      } else {
        // else, run dfs for each child of the vertex and detect cycle
        val children = adjList(vertex)
        children.forall {
          child =>
            dfs(child, visitedSet + vertex)
        }
      }

    }

    adjList.keys.forall(dfs(_))
  }

}
