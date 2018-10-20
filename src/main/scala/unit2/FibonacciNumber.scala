package unit2

/**
  *获取第n个斐波那契数,序列开始为：0，1，1，2，3，5
  *
  * 分析：1.先判断一下n=1 和 n = 2的情况
  * 递归的思想 就是自己调用自己，并且要考虑程序退出的情况
  */
object FibonacciNumber {
//  @annotation.tailrec
  def fib(n:Int):Int = {
    if (n==1) 0
    else if (n==2) 1
    else fib(n-1) + fib(n-2)
  }
  def main(args: Array[String]): Unit = {
    println(fib(20))
  }
}
