object SortByParity {
    def sortArrayByParity(A: Array[Int]): Array[Int] = {
        
        val len = A.length 
        
        @inline
        def swap(source : Int, dest : Int) = {
            val temp = A(source)
            A(source) = A(dest)
            A(dest) = temp
        }
        
        @scala.annotation.tailrec
        def helper(idx : Int = 0,
                   wall : Int = 0) : Unit = {
            if(idx < len){
                if(A(idx) % 2 == 0) {
                    swap(wall, idx)
                    helper(idx + 1, wall + 1)   
                } else {
                    helper(idx + 1, wall)
                }
            }
            
        }
        
        helper()
        A
    }

    def main(args : Array[String]) : Unit = {
        val arr = Array(3, 1, 2, 4)
        val ans = sortArrayByParity(arr)

        assert(ans == Array(2, 4, 3, 1))
    }
}