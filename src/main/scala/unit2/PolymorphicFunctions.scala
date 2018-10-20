package unit2

import sun.awt.SunHints.Key

/**
  * 这个是一个多态函数的例子
  */
object PolymorphicFunctions {
  //在数组中查找一个key
  def findFirst(ss:Array[Int],key:Int):Int = {
    def loop(n:Int):Int = {
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else loop(n+1)
    }
    loop(0)
  }

  /**
    *但是上面的代码有一个问题，就是只能传int类型的代码，那么我们需要判断其他类型的时候怎么办？
    * 在平常写java的时候也许我们会写一对重载方法，但是scala就不用了，可以写多态函数了。
    *
    */
  def findFirstPolymorphic[A](as:Array[A],p : A => Boolean):Int ={
    def loop(n:Int):Int = {
      if (n >= as.length) -1
      else if (p(as(n))) n
      else loop(n+1)
    }
    loop(0)
  }
  def p[A](s :A):Boolean = {
    s.isInstanceOf[Int]
  }
  def main(args: Array[String]): Unit = {
    val ss = Array(1,2,3,4,5,6,7)
    println(findFirst(ss,7))
    println(findFirstPolymorphic[Int](ss,p))
  }
}
