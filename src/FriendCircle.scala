object FriendCircle {

  def main(args: Array[String]): Unit = {
    val gridInput =
      """|110
         |110
         |001""".stripMargin

    val grid = getGraph(gridInput)
    val answer = findCircleNum(grid)
    println(answer)

  }

  def getGraph(input: String): Array[Array[Int]] = {
    input.split("\n").map(_.map(_.asDigit).toArray)
  }


  // beats 100%
  def findCircleNum(grid: Array[Array[Int]]): Int = {
    // union find on all persons
    // return the number of unique parents
    val m = grid.length
    if (m == 0) {
      0
    } else {
      val parent = (0 until m).toArray

      def findParent(person: Int): Int = {
        if (parent(person) == person) {
          person
        } else {
          // path compression
          val parentOfPerson = findParent(parent(person))
          parent(person) = parentOfPerson
          parentOfPerson
        }
      }

      //  without path compression
      //  @scala.annotation.tailrec
      //  def findParent(person: Int): Int = {
      //    if (parent(person) == person) {
      //      person
      //    } else {
      //      findParent(parent(person))
      //    }
      //  }

      for {
        // iterate each person
        person <- 0 until m
        // due to symmetry property
        friend <- 0 until person
        if grid(person)(friend) == 1} {
        val parentOfPerson = findParent(person)
        val parentOfFriend = findParent(friend)
        parent(parentOfFriend) = parentOfPerson
      }

      // get number of unique parents
      (0 until m).foldLeft(Set[Int]()) {
        case (acc, person) =>
          acc + findParent(person)
      }.size

    }
  }

}
