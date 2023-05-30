import scala.math.log

object Util {

  def max[A](as: List[A], comp:(A,A)=>Int):A= {
    var mx=as.head
    if(as.size==1)
      return mx
    val a=max(as.tail,comp)
    if(comp(mx,a)<0)
      mx=a
    mx
  }

  def map[A,B,C](as: List[A],fab:A=>B,fbc:B=>C):List[C]=
    as.map(fab).map(fbc)

  def isSorted[A](as: List[A], ordered: (A,A) => Boolean): Boolean={
    if(as.size==1)
      return true
    if(ordered(as.head,as.tail.head))
      isSorted(as.tail,ordered)
    else
      false
  }

  def probs(xs:Array[Double]):Array[Double]=
    xs.map(xi=>xs.count(xj=>xi==xj)/xs.size.toDouble)

  def entropy(xs:Array[Double]):Double=
    -(xs zip probs(xs)).distinct.map(pi=>pi._2*log(pi._2)/log(2)).sum

  def mu(xs:Array[Double]):Double =
    (xs zip probs(xs)).distinct.map{Function.tupled(_*_)}.sum

  def variance(xs:Array[Double]):Double =
    (xs zip probs(xs)).distinct.map{Function.tupled((xi,pi)=>pi*(xi-mu(xs))*(xi-mu(xs)))}.sum

  def zscore(xs:Array[Double],x:Double):Double=
    (x-mu(xs))/Math.sqrt(variance(xs))

  def cov(xs:Array[Double] , ys:Array[Double]):Double =
    mu((xs zip ys).map(Function.tupled((xi,yi)=>xi*yi))) - mu(xs)*mu(ys)

  def pearson(xs:Array[Double] , ys:Array[Double]):Double =
    cov(xs,ys)/(Math.sqrt(variance(xs))*Math.sqrt(variance(ys)))
}
