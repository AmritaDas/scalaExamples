package example

object recfun {
    def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }
    
  
  
  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c==0) 1  //left edge
      else if(r==c) 1  //right edge
      else pascal(c-1, r-1) + pascal(c, r-1)  //insides
    }
  
  /**
   * Exercise 2
   */
    def balance1(chars: List[Char]): Boolean = {
      var count = 0
      for(c <- chars){
        if(c == '(')
          count = count+1
        else if(c == ')')
          count = count-1
        if(count < 0) false
      }
      if(count != 0)  false
      else true
    }
    
    def balance(chars: List[Char]): Boolean = {
      def bal (count:Int, chars: List[Char]): Int = {
        if(count < 0) -1
        else if(chars.isEmpty) count
        else if(chars.head == '(')
          bal(count+1, chars.tail)
        else if(chars.head == ')')
          bal(count-1, chars.tail)
        else bal(count, chars.tail)
      }
      
      (bal(0, chars) == 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      def countC(money: Int, coins: List[Int], num: Int): Int = {
        if(money==0) 1
        else if (money < 0) 0
        else if (money > 0 && num <= 0) 0
        else{
          countC(money, coins, num-1) + countC(money-coins(num-1), coins, num)
        }
      }
      countC(money, coins, coins.length)
    }
    
}
