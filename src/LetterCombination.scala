object LetterCombination {
    def letterCombinations(digits: String): List[String] = {
        val charMap = Map('2' -> List('a', 'b', 'c'),
                          '3' -> List('d', 'e', 'f'),
                          '4' -> List('g', 'h', 'i'),
                          '5' -> List('j', 'k', 'l'),
                          '6' -> List('m', 'n', 'o'),
                          '7' -> List('p', 'q', 'r', 's'),
                          '8' -> List('t', 'u', 'v'),
                          '9' -> List('w', 'x', 'y', 'z'))
        val len = digits.length
        
        def helper(idx : Int,
                   buffer : String,
                   acc : List[String]) : List[String] = {
            if (idx == len){
                acc :+ buffer
            } else {
                charMap(digits(idx)).foldLeft(acc){
                    case (newAcc, selectChar) =>
                        helper(idx + 1, buffer + selectChar, newAcc)
                }
            }
        }
        
        if(len > 0)
            helper(0, "", List[String]())
        else
            List[String]()
    }
}
