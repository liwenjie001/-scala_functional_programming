package unit2

object MyModule {
  //简单写一个求绝对值的函数。
  def abs(n : Int) : Int = {
    if(n<0) -n
    else n
  }
  // %d 这里面代表的是int 代表的是占位符。
  private def fromatAbs(x : Int) = {
    val msg = "The abasolute value of %d is %d "
    // 这个是把这个字符串给其占位符传入参数
    msg.format(x,abs(x))
  }
  private def fromatFactorial(x : Int) = {
    val msg = "The abasolute value of %d is %d "
    // 这个是把这个字符串给其占位符传入参数
    msg.format(x,factorial(x))
  }
  def factorial(n:Int):Int = {
    @annotation.tailrec
    def go(n:Int,acc:Int):Int = {
      if (n<=0) acc
      else go(n-1,n*acc)
    }
    go(n,1)
  }

  def main(args: Array[String]): Unit = {
    println(fromatAbs(-42))
  }
}
