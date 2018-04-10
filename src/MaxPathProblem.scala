object MaxPathProblem {

  // (m-1 + n-1)!/(m-1)!(n-1)!
  //  def numberOfPaths(m: Int, n: Int): Int = {
  //    if (m == 1 || n == 1) {
  //      1
  //    } else {
  //      numberOfPaths(m - 1, n) +
  //        numberOfPaths(m, n - 1) +
  //        numberOfPaths(m - 1, n - 1)
  //    }
  //  }

  lazy val numberOfPaths: ((Int, Int)) => BigInt = Util.memoize {
    case (1, _) => 1
    case (_, 1) => 1
    case (m, n) => numberOfPaths(m - 1, n) +
      numberOfPaths(m, n - 1) +
      numberOfPaths(m - 1, n - 1)
  }


  def memoNumOfPaths(m: Int, n: Int): Int = {
    type I = (Int, Int) // input and cache key type
    type O = Int // output type
    type DP = Memo[I, I, O]

    implicit def encode(key: (Int, Int)): (Int, Int) = key

    lazy val fn: DP = Memo {
      case (1, _) => 1
      case (_, 1) => 1
      case (x, y) =>
        fn(x - 1, y) + fn(x, y - 1) + fn(x - 1, y - 1)
    }
    fn(m, n)
  }

  val memoMap = scala.collection.mutable.Map((1, 1) -> 1)

  def numberOfPaths2(m: Int, n: Int): Int = {
    if (m == 1 || n == 1) {
      1
    } else {
      if (memoMap.contains((m, n)) || memoMap.contains((n, m))) {
        memoMap.getOrElse((m, n), memoMap(n, m))
      } else {
        val a = numberOfPaths2(m - 1, n)
        val b = numberOfPaths2(m, n - 1)
        val c = numberOfPaths2(m - 1, n - 1)
        memoMap += ((m - 1, n) -> a)
        memoMap += ((m, n - 1) -> b)
        memoMap += ((m - 1, n - 1) -> c)
        a + b + c
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val m = 3
    val n = 2
    println(s"The matrix is of size of $m x $n")

    // println(numberOfPaths(m, n))
    println(s"The number of paths to reach to bottom is : ${numberOfPaths(m, n)}")
    assert(numberOfPaths2(m, n) == numberOfPaths(m, n))
    assert(memoNumOfPaths(m, n) == numberOfPaths(m, n))
  }

}
