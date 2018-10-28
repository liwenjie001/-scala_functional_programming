package unit3

/**
  * author: Mr.Li 
  * create: 2018-10-28 13:59 
  * description: 
  **/
object ListDemo2 {
  def main(args: Array[String]): Unit = {

  }
  //数据共享通常可以实现更好的性能
  // 删除前n个元素
  def drop[A](l:List[A],n:Int):List[A] = (1,n) match {
    case (Nil , _) => Nil
    case (_,0) => l
    case (Cons(_,tail),_) => drop(tail,n-1)
  }
}
