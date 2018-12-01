package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      var limit = 20

      if (row == 10) limit = 17 else
      if (row >= 9) limit = 18 else
      if (row >= 5) limit = 19

      for(space <- row to limit) {
        print(" ")
      }

      for (col <- 0 to row) {
        val p = pascal(col, row)
        var s = " "
        print(p + s)
      }
      println()
    }

    println(countChange(300, List(5,10,20,50,100,200,500)))
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int =
      if (c == 0 || c == r) 1 else pascal(c-1, r-1) + pascal(c, r-1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {

      def balancing(chars: List[Char], open : Int) : Boolean = {
        if (chars.isEmpty || open < 0) open == 0
        else if (chars.head.equals('(')) balancing(chars.tail, open + 1)
        else if (chars.head.equals(')')) balancing(chars.tail, open - 1)
        else
          balancing(chars.tail, open)
      }

      balancing(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
        if (coins.isEmpty) 0
        else if (money == 0) 1
        else if (money < 0) 0
        else
          countChange(money - coins.head, coins) +
          countChange(money, coins.tail)
    }
  }