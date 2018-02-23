object MaxProductPair {


  def getMaxProductPair(array: Array[Int]): (Int, Int) = {

    val init = Int.MinValue
    import math.abs
    val (firMax, secMax, firMin, secMin) = array.foldLeft(init, init, init, init) {
      case ((fMax, sMax, fMin, sMin), present) =>
        // println(s"Before : ($fMax, $sMax, $fMin, $sMin) and $present")
        val (newFMax, newSMax) = if (present > fMax) {
          (present, fMax)
        } else if (present > sMax) {
          (fMax, present)
        } else {
          (fMax, sMax)
        }

        val (newFMin, newSMin) = if (present < 0) {
          if (abs(present) > abs(fMin)) {
            (present, fMin)
          } else if (abs(present) > abs(sMin)) {
            (fMin, present)
          } else {
            (fMin, sMin)
          }
        } else {
          (fMin, sMin)
        }

        // println(s"After : ($newFMax, $newSMax, $newFMin, $newSMin) and $present")
        (newFMax, newSMax, newFMin, newSMin)
    }

    if ((firMax * secMax) > (secMin * firMin)) {
      (firMax, secMax)
    } else {
      (secMin, firMin)
    }
  }

  def main(args: Array[String]): Unit = {

    val array = Array(1, 4, 3, 6, 7, 0, -7, -8)
    println(s"The given array is : ${array.mkString(", ")}")

    val (p1, p2) = getMaxProductPair(array)
    println(s"The pair is $p1 and $p2 and maxProduct is ${p1 * p2}")

  }

}
