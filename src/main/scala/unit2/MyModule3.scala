package unit2

/**
  * 高阶函数的应用
  */
object MyModule3 {
  //简单写一个求绝对值的函数。
  def abs(n : Int) : Int = {
    if(n<0) -n
    else n
  }
  // %d 这里面代表的是int 代表的是占位符。
  private def fromatResult(name :String,x : Int,f :Int =>Int) = {
    val msg = "The %s value of %d is %d "
    // 这个是把这个字符串给其占位符传入参数
    msg.format(name,x,f(x))
  }
  /*private def fromatFactorial(x : Int) = {
    val msg = "The abasolute value of %d is %d "
    // 这个是把这个字符串给其占位符传入参数
    msg.format(x,factorial(x))
  }*/
  def factorial(n:Int):Int = {
    @annotation.tailrec
    def go(n:Int,acc:Int):Int = {
      if (n<=0) acc
      else go(n-1,n*acc)
    }
    go(n,1)
  }
  
  def main(args: Array[String]): Unit = {
    //高阶函数，就是把一个函数名当参数传入另一个函数当中。那么这个函数的参数讲在被调用的函数体中展现出来。
    println(fromatResult("绝对值",-42,abs))
  }
}
