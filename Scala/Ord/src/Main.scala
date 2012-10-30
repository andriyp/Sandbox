abstract class Eq[T] {
  def eql(l: T, r: T): Boolean
  
  implicit class EqOps(x: T) {
    def === =   eql(x, _:T)
    def =/= = ! eql(x, _:T)
  }
}

abstract class Ord[T](implicit val $eq: Eq[T]) {
  import $eq._
    
  def leq(l: T, r: T): Boolean =
    compare(l, r) != 1
  
  def compare(l: T, r: T): Int =
    if      (l === r)  0
    else if (l  <= r) -1
    else               1      
    
  implicit class OrdOps(x: T) {
    def <= = leq(x, _:T)
    def <  = compare(x, _:T) == -1    
    def >  = compare(x, _:T) ==  1
    def >= = compare(x, _:T) != -1
  }
}

object Main extends App {
    
  def quux[T](xs: List[T], ys: List[T])(implicit $ord: Ord[List[T]]) = {
    import $ord._
    import $ord.$eq._
    
    if      (xs  <  ys) xs
    else if (xs === ys) List(xs.head, ys.head)
    else                ys
  }
  
  implicit val intEq = new Eq[Int] {
    def eql(a: Int, b: Int) = a == b
  }  
  
  implicit val intOrd = new Ord[Int] {
    override def leq(a: Int, b: Int) = a <= b
  }
    
  implicit def listEq[T](implicit $eq: Eq[T]) = new Eq[List[T]] {
    def eql(l: List[T], r: List[T]) = l.corresponds(r)($eq.eql)
  }
  
  implicit def listOrd[T: Eq](implicit $ord: Ord[T]) = new Ord[List[T]] {
    override def leq(l: List[T], r: List[T]) = l.corresponds(r)($ord.leq)
  }

  println(quux(List(2, 4), List(2, 3)))
  // => List(2, 3)
}