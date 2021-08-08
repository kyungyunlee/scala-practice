package recfun
import scala.annotation.tailrec

object RecFun extends RecFunInterface:

  def main(args: Array[String]): Unit =
    println("Pascal's Triangle")
    for row <- 0 to 10 do
      for col <- 0 to row do
        print(s"${pascal(col, row)} ")
      println()

    val input_str = "())("
    println(balance(input_str.toList))

    println(countChange (4, List(3)))
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || c == r || r == 0 || r ==1 ) then 1 else pascal(c-1, r-1) + pascal(c, r-1)

  /**
   * Exercise 2
   */

//  def balance(chars: List[Char]): Boolean =
//    def inner_balance(chars: List[Char], stack: List[Char]) : Boolean =
//      if (chars.isEmpty)
//        if (stack.isEmpty)
//          return true
//        else
//          return false
//      else if (chars.head == ')')
//        if (stack.isEmpty)
//          return false
//        else if  (stack.last == '(')
//          return inner_balance(chars.tail, stack.dropRight(1));
//        else
//          return false
//      else if (chars.head == '(')
//        val new_stack = stack :+ chars.head
//        return inner_balance(chars.tail, new_stack)
//      else
//        inner_balance(chars.tail, stack)
//
//    inner_balance(chars, "".toList)

  def balance(chars: List[Char]) : Boolean = {
    @tailrec
    def inner_balance(chars:List[Char], counter: Int) : Int = {
      if (chars.isEmpty || counter < 0) counter
      else if (chars.head == '(') inner_balance(chars.tail, counter+1)
      else if (chars.head == ')') inner_balance(chars.tail, counter-1)
      else inner_balance(chars.tail, counter)
    }
    inner_balance(chars, 0) == 0
  }

  /**
//   * Exercise 3
//   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }


