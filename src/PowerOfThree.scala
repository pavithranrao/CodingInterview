object PowerOfThree {

  def main(args: Array[String]): Unit = {
    val inputs = Array(27, 0, 9, 45)
    val outputs = inputs.map(isPowerOfThree)

    assert(outputs sameElements Array(true, false, true, false))
  }

  // beats 100%
  def isPowerOfThree(n: Int): Boolean = {
    import Math.log10
    val log3OfN = log10(n) / log10(3)
    log3OfN.isValidInt
  }

}
