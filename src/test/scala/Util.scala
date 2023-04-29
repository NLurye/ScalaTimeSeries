import Util.variance

import scala.math.{log, pow}

object Util {

  // max
  def max[A](list: List[A], comparator: (A, A) => Int): A = {
    list.reduceLeft((x, y) => if (comparator(x, y) > 0) x else y)
  }

  // map
  def map[T, V](list: List[T], x: T => T, y: T => V): List[V] =
    list.map(t => y(x(t)))

  // isSorted
  def isSorted[A](lst: List[A],comp: (A, A) => Boolean): Boolean =
    lst.isEmpty || lst.sliding(2).forall(l => comp(l.head, l(1)))


  // probs
  def probs(xs: Array[Double]): Array[Double] = {
    val total = xs.length
    xs.map(x => xs.count(_ == x) / total.toDouble)
  }

  def countUniqueOccurrences[T](xs: Array[T]): Map[T, Int] = {
    xs.groupBy(identity).view.map { case (k, v) => k -> v.length }.toMap
  }

  // entropy
  def entropy(xs: Array[Double]): Double = {
    val uniqueCounts = countUniqueOccurrences(xs)
    val sum =  uniqueCounts.values.sum.toDouble
    val probabilities = uniqueCounts.values.map(x => x / sum)
    -probabilities.map(p => p * (math.log(p) / math.log(2))).sum
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
    val xMean = mu(xs)
    val yMean = mu(ys)
    mu((xs zip ys).map{ case (x, y) => (x - xMean) * (y - yMean)})
  }

  // pearson
  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    val n = xs.length
    val xMean = mu(xs)
    val yMean = mu(ys)
    val stdDevX = math.sqrt(xs.map(x => math.pow(x - xMean, 2)).sum / n)
    val stdDevY = math.sqrt(ys.map(y => math.pow(y - yMean, 2)).sum / n)
    val cov = (xs zip ys).map{case (x, y) =>  (x - xMean) * (y - yMean)}.sum / n
    cov / (stdDevX * stdDevY)
  }
}
