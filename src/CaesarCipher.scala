object CaesarCipher {

  @inline
  def rotateChar(inputChar: Int, rotateBy: Int, isLowerCase: Boolean): Char = {
    val ans = inputChar - rotateBy
    if ((isLowerCase && (ans < 65)) || (!isLowerCase && (ans < 97))) {
      (26 + ans).toChar
    } else {
      ans.toChar
    }
  }

  def decrypt(encrypted_message: String, code: Seq[Int]): String = {
    val codeGen: Iterator[Int] = Util.cyclicIterator(code)
    encrypted_message.map {
      x =>
        (x >= 65 && x <= 90, x >= 97 && x <= 122) match {
          case (true, _) =>
            rotateChar(x, codeGen.next % 48, isLowerCase = true)
          case (_, true) =>
            rotateChar(x, codeGen.next % 48, isLowerCase = false)
          case _ => x
        }
    }.mkString("")
  }

  def main(args: Array[String]): Unit = {
    val message = "Your friend, Alice"
    val encryptedMessage = "Atvt hrqgse, Cnikg"

    val zipped = message.filter(_.isLetter).zip(encryptedMessage.filter(_.isLetter))
    val difference = zipped.map {
      x =>
        val diff = x._2 - x._1
        if (diff < 0) {
          26 + diff
        } else {
          diff
        }
    }
    // difference.zipWithIndex.foreach(x => print(s"$x "))
    val code = difference.slice(6, 13)
    val inputMessage = "Bjj rwkcs dwpyp fwz ovors wxjs vje tcez fqg"
    val outputMessage = "The quick brown fox jumps over the lazy dog"

    val decryptedMessage = decrypt(inputMessage, code)

    assert(decryptedMessage == outputMessage)
    println(s"The decrypted message is : $decryptedMessage")
  }

}
