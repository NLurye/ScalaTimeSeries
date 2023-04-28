import Util.variance

import scala.math.{log, pow}

object Util {

  // max
  def max[A](list: List[A], comparator: (A, A) => Int): A = {
    list.reduceLeft((x, y) => if (comparator(x, y) > 0) x else y)
  }

  // map
  def map[T, U](list: List[T], x: T => U, y: T => U): List[U] =
    list.flatMap(t => List(x(t), y(t)))

  // isSorted
  def isSorted[A](lst: List[A], f: (A, A) => Boolean): Boolean = lst match {
    case Nil => true
    case _ :: Nil => true
    case x :: y :: tail if f(x, y) => isSorted(y :: tail, f)
    case _ => false
  }

  // probs
  def probs(xs: Array[Double]): Array[Double] = {
    val total = xs.sum
    xs.map(_ / total)
  }

  // entropy
  def entropy(xs: Array[Double]): Double = {
    val total = xs.sum
    xs.map(x => if (x == 0) 0 else -x / total * math.log(x / total)).sum
  }

  // mu
  def mu(xs: Array[Double]): Double = xs.sum / xs.length

  // variance
  def variance(xs: Array[Double]): Double = {
    val m = mu(xs)
    xs.map(x => math.pow(x - m, 2)).sum / xs.length
  }

  // zscore
  def zscore(xs: Array[Double], x: Double): Double = {
    val mean = xs.sum / xs.length
    val stdDev = math.sqrt(xs.map(d => math.pow(d - mean, 2)).sum / xs.length)
    (x - mean) / stdDev
  }


  // cov
  def cov(xs: Array[Double], ys: Array[Double]): Double = {
    require(xs.length == ys.length, "Arrays must have same length")
    val n = xs.length
    val xMean = xs.sum / n
    val yMean = ys.sum / n
    val covSum = (xs zip ys).map{ case (x, y) => (x - xMean) * (y - yMean)}.sum
    covSum / (n - 1)
  }

  // pearson
  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    val n = xs.length
    val meanX = xs.sum / n
    val meanY = ys.sum / n
    val stdDevX = math.sqrt(xs.map(x => math.pow(x - meanX, 2)).sum / n)
    val stdDevY = math.sqrt(ys.map(y => math.pow(y - meanY, 2)).sum / n)
    val cov = (xs, ys).zipped.map(_ * _).sum / n - meanX * meanY
    cov / (stdDevX * stdDevY)
  }
}
