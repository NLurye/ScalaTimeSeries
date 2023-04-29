class Line(ps:Array[Point]) {
  private val xs: Array[Double] = ps.map(p => p.x)
  private val ys: Array[Double] = ps.map(p => p.y)
  val a: Double = get_a()
  val b: Double = get_b()

  // distance to a point
  def dist(p: Point): Double = {
    Math.abs(p.y-f(p.x))
  }

  def get_a(): Double = {
    Util.cov(xs,ys) / Util.variance(xs)
  }

  def get_b(): Double = {
    Util.mu(ys) - a* Util.mu(xs)
  }

  // equation of the line
  def f(x: Double): Double = a * x + b
}
