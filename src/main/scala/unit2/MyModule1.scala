package unit2

/**
  * 高阶函数：把函数传给函数
  */
object MyModule1 {
  /**
    * 迂回做法：使用循环方式
    * 首先来个阶乘
    * 这个是递归的尾调用，scala会给其优化的。
    * annotation.tailrec : 这个注解是检查是否是尾调用的？如果不是尾调用的话，会报错
    */
  def factorial(n:Int):Int = {
    @annotation.tailrec
    def go(n:Int,acc:Int):Int = {
      if (n<=0) acc
      else go(n-1,n*acc)
    }
    go(n,1)
  }
  def main(args: Array[String]): Unit = {
    println(factorial(5))
  }
}
