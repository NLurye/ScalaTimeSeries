class Line(ps:Array[Point]) {
  private val xs: Array[Double] = ps.map(p => p.x)
  private val ys: Array[Double] = ps.map(p => p.y)
  val a: Double = Util.cov(xs,ys) / Util.variance(xs)
  val b: Double = Util.mu(ys) - a* Util.mu(xs)

  // distance to a point
  def dist(p: Point): Double = {
    scala.math.abs(p.y-f(p.x))
  }

  // equation of the line
  def f(x: Double): Double = a * x + b
}
