package unit2

/**
  * 这个是练习柯里化的例子：
  * 把带有两个参数的函数f转换成只有一个参数的部分函数，只要编译通过就可以
  */

object PractiseDemo1 {
  def main(args: Array[String]): Unit = {

  }
  //这种函数要高阶函数要逆推从返回值下手来一步一步写出具体的实现
  //这个是柯里化的
  def curry[A,B,C](f:(A,B) => C):A => (B => C) = {
//    (B=> C)
    (a:A) => ((b:B)=> f(a,b))
  }
  /**
    * 首先传入的参数为两个， (a:A,f:(A, B) 其中一个为函数f(A, B) 而 函数 f(A,B) 的主体是 C
    * f:(A, B) => C 它代表的是返回值为 c 的函数
    * 而这个函数的返回值是类型为： B=>C 的函数
    */
  def partiall[A,B,C](a:A,f:(A, B) => C):B=>C =
    (b:B) => f(a,b)
  // 接下来我们来个反柯里化的
  //注意因为=>是向右结合的，A=>B=>C 相当于 A=>（B=>C）
  // 这个就要注意括号了，并且要分析传入参数的顺序，还要注意匿名函数的利用。
  def uncurry[A,B,C](f:A=>(B=>C)):(A, B) => C = {
    (a:A,b:B) => f(a)(b)
  }
  //实现一个高阶函数，可以组合两个函数为一个函数。
  //在Function1(带有一个参数的函数接口当中)，提供了compose方法。要对函数f 和 g 进行组合 只需要简单的写成
  //f compose g 既可 同时还提供了 andThen 方法 ，f andThen g 相当于 g compose f ;
  def compose [A,B,C](f:B => C ,g:A =>B ):A=>C = {
    a:A => f(g(a))
  }
  val f = (x:Double) => math.Pi /2 - x
  val cos = f andThen math.sin

}
