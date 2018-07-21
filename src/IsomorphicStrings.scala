import scala.annotation.tailrec

object IsomorphicStrings {

  def main(args: Array[String]): Unit = {


  }

  // beats 93%
  def isIsomorphic(s: String, t: String): Boolean = {
    val sa = new Array[Int](128)
    val ta = new Array[Int](128)
    val len = s.length

    @tailrec
    def check(idx: Int = 0): Boolean = {
      if (idx == len) {
        true
      } else {
        if (sa(s(idx)) != ta(t(idx))) {
          false
        } else {
          sa(s(idx)) = idx + 1
          ta(t(idx)) = idx + 1
          check(idx + 1)
        }
      }
    }

    check()
  }

  def isIsomorphic2(s: String, t: String): Boolean = {
    def charMap(str: String): Seq[Int] = str.map(x => str.indexOf(x))

    charMap(s) == charMap(t)
  }

}
