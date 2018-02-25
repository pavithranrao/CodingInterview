object RodCutting {

  def getMaxPrice(rodPrice: Map[Int, Int], rodLength: Int): (Int, Array[Int]) = {

    // extract rod lengths
    val rods = rodPrice.keys.toArray.sorted

    // initialise C to be [-∞ ... -∞]
    val C = Array.fill(rodLength + 1)(Int.MinValue)
    // store the rod prices given in their
    // respective indices
    rodPrice.foreach {
      case (key, value) =>
        C(key) = value
    }

    // initialise S to be [-1 -1 ... -1]
    // S is used for tracing the output
    // if S(i) == -1 -> rod of length i cannot be cut further
    // else S(i) is to be cut into S(i) and i - S(i)
    val S = Array.fill(rodLength + 1)(-1)

    // iterate from 1 to rodLength and calculate the
    // maximum price for each rodLength
    for (rod <- 1 to rodLength) {
      // iterate on all possible cuts
      for (cut <- rods.filter(_ < rod)) {
        val newPrice = rodPrice.getOrElse(cut, Int.MinValue) + C(rod - cut)
        // if newPrice is greater than the C(rod)
        // the newPrice is stored to track max and
        // the cut is stored in S(rod) for trace
        if (newPrice > C(rod)) {
          C(rod) = newPrice
          S(rod) = cut + 1
        }
      }
    }

    def printTrace(rodCuts: Array[Int], idx: Int = rodLength): Array[Int] = {
      if (S(idx) == -1) {
        rodCuts :+ idx
      } else {
        // the cut for S(idx) is stored in the output trace
        // output => rodCuts :+ S(idx)
        // the remaining is recursively called with
        // present rod length - cut
        // newLength => idx - S(idx)
        printTrace(rodCuts :+ S(idx), idx - S(idx))
      }
    }

    // printTrace prints the cuts for the given rodLength
    // initially it take the S(rodLength) as the start position
    val rodCuts = printTrace(Array[Int]())
    (C.last, rodCuts)
  }

  def main(args: Array[String]): Unit = {

    val rodPrice = Map(1 -> 1, 2 -> 5, 3 -> 8, 4 -> 9,
      5 -> 10, 6 -> 17, 8 -> 20)
    val rodLength = 10

    val (maxPrice, rodCuts) = getMaxPrice(rodPrice, rodLength)
    println(s"The maximum possible price is $maxPrice")
    println(s"The cuts for $rodLength are ${rodCuts.mkString(", ")}")

    assert(rodCuts.sum == rodLength)
  }

}
