object BattleShip {

  def main(args: Array[String]): Unit = {
    val ans = solution(4, "1B 2C,2D 4D", "2B 2D 3D 4D 4A")
    assert(ans == (1, 1))
  }


  def solution(n: Int, s: String, t: String): String = {
    val ships = parseShips(s, n * n)
    val hits = parseHits(t, n * n)

    val (touch, sunk) = ships.foldLeft((0, 0)) {
      case ((touched, sunken), current) =>
        val touching = current.getHits(hits)
        if (touching > 0) {
          if (touching == current.getSize) {
            (touched, sunken + 1)
          } else {
            (touched + 1, sunken)
          }
        } else {
          (touched, sunken)
        }
    }

    s"$sunk,$touch"
  }

  def parseShips(ships: String, maxShips: Int): Array[Ship] = {
    val shipCoordinates = ships.split(",")
    shipCoordinates.map {
      shipCoord =>
        val coord = shipCoord.split(" ")
        val topLeft = new Point(coord(0))
        val bottomRight = new Point(coord(1))
        new Ship(topLeft, bottomRight)

    }
  }

  def parseHits(hits: String, maxHits: Int): Array[Point] = {
    val hitCoordinates = hits.split(" ")
    hitCoordinates.map(new Point(_))
  }

  class Point(coordinate: String) {
    val x: Int = coordinate.toUpperCase()(1) - 'A'
    val y: Int = coordinate(0).asDigit

    def greaterOrEqual(other: Point): Boolean = {
      x >= other.x && y >= other.y
    }
  }

  class Ship(topLeft: Point, bottomRight: Point) {

    def getSize: Int = {
      (Math.abs(topLeft.x - bottomRight.x) + 1) *
        (Math.abs(topLeft.y - bottomRight.y) + 1)
    }

    def getHits(shots: Array[Point]): Int = {
      shots.count(shot => shot.greaterOrEqual(topLeft) && bottomRight.greaterOrEqual(shot))
    }
  }

}
