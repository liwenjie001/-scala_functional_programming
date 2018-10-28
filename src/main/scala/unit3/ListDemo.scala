package unit3

/**
  * author: Mr.Li 
  * create: 2018-10-27 22:11 
  * description: 这个是模拟 scala 的List
  **/

//sealed 代表的是只能在同一个文件当中对这个进行继承
sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head:A,tail:List[A]) extends List[A]

object List{
  def sum(ints :List[Int]):Int = ints match {
    case Nil => 0 // 空列表累加值为 0
    //对一个头部是x的列表进行累加，这个过程是用x加上
    //该列表剩余部分的累加值
    case Cons(x, xs) => x + sum(xs)
  }
  //乘法 这个应该是 偏函数 的类型的。
  def product(ds:List[Double]):Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  // 可变参数
  def apply[A](as:A*):List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head,apply(as.tail:_*))
  }
  //数据共享
  // 这个是代表的是删除第一个元素
  def tail[A](l:List[A]):List[A]  = l match {
    case Nil => Nil
    case Cons(head,tail) => tail
  }
  //替换当前中的第一个元素
  // 就是替换第一个元素为其他的元素。
  def setHead[A](l:List[A],h:A):List[A] = l match {
    case Nil => Nil
    case Cons(_, tail) => Cons(h,tail)
  }
  //数据共享通常可以实现更好的性能
  // 删除前n个元素
  def drop[A](l:List[A],n:Int):List[A] = (l,n) match {
    case (Nil,_) => Nil
    case (_,0) => l
    case (Cons(_,tail),_) => drop(tail,n-1)
  }
  // 实现 dropWhile 函数 ，删除列表中前缀全部符合判定的元素。
  def dropWhile[A](l:List[A],f:A => Boolean ):List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail,f) else l
  }
  // 利用数据共享的特性将一个列表的所有元素加到另一个列表的后面：
  //
  def append[A](a1:List[A],a2:List[A]) : List[A] = a1 match {
    case Nil  => Nil
    case Cons(head, tail) => Cons(head,append(tail,a2)) // 这块这个思想是真的好 。
  }
  // 实现一个 init 函数，返回一个列表，它包含原列表中除了最后一个元素之外的所有元素。
  //比如：传入一个 List(1,2,3,4) 给 init(1,2,3) ,为什么这个函数不能实现同 太累 一样的常量级的开销呢 ?
  def init[A](l:List[A]):List[A] = l match {
    // 只有两种情况会返回Nil 为什么呢？  如果元素只有一个 那么也低必须返回一个 Nil的。
    case Nil => Nil
    case Cons(head, Nil) => Nil
    // 这块连续的回调 最后调用到 case Cons(head, Nil) => Nil 就把 最后一个给其删除了。
    case Cons(head, tail) => Cons(head,init(tail))
  }
  //一般来讲，当函数定义包含多个参数组的时，参数组里面的类型信息从左到右传递的。
  // 这里第一个参数组确定A类型参数为Int ,所以 x => x < 4 里的类型标注可以不需要。
  // 我们常通过将函数参数分组排序成多个参数列表，来最大化地利用类型推导的。

  //把f参数独立出来，放在 as 和 z 参数的后边，是为了让类型系统能推到出f的输入类型。
  def foldRight[A,B](as:List[A],z:B)(f:(A,B)=>B):B =  as match {
    case Nil => z
    case Cons(x, xs) => f(x,foldRight(xs,z)(f))
  }
  def sum2(ns:List[Int]) =
      foldRight(ns,0)((x,y)=>x+y)

  def product2(ns:List[Double]) =
    foldRight(ns,1.0)(_ * _)

  // foldRight 函数不是面向特定的一种元素类型，泛化的返回值不必一定与 list 中的元素类型相同，
  // 一种描述 foldRigth 是做什么的方式是用 z 和 f 替换列表Nil 和 Cons的构造器。
  // 这是求 list的长度
  def lengt[A](l:List[A]):Int = foldRight(l,0)((_,len)=>len + 1 )
  //这个是折叠，但是这块没有使用尾递归，接下来要解决的就是要使用 尾递归 来解决。
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B = as match {
    case Nil => z
    case Cons(x, xs) => f(x,foldLeft(xs,z)(f))
  }
  // 这个是用尾递归写的 ，写的很明白。
  def foldLeft[A,B](as:List[A],z:B)(f:(B,A)=>B):B =  {
    def flodIter(as:List[A],accumulator:B):B = as match {
      case Nil => accumulator
      case Cons(head, tail) => flodIter(tail,f(accumulator,head))
    }
    flodIter(as,z)
  }
  def sum3(ns:List[Int]) =
    foldLeft(ns,0)(_+_)
  // 正好相反的
  def lengt2[A](l:List[A]):Int = foldLeft(l,0)((len,_)=>len+1)
  // 取反啊 list(1,2,3) list(3,2,1)
  /*def test_reverse[A](l:List[A]):List[A] = l match {
    case Nil => Nil
    case Cons(head, tail) => foldRight()
  }*/









}

