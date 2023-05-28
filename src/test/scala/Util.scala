import scala.collection.mutable
import scala.math.log

// max
object Util {
  // Define a generic function called "max" that takes a List of type A and a function that takes two elements of type A and returns an integer
  def max[A] (as: List[A], comp: (A, A) => Double): A = {
    // Initialize a variable called "max" to the first element of the list
    var max = as.head
    // Iterate through each element of the list.
    as.foreach(a => {
      // Compare the current element "a" with "max" using the "comp" function
      // If "a" is greater than "max", set "max" equal to "a"
      if (comp(max, a) < 0)
        max = a
    })
    // Return the maximum element
    max
  }


  // map
  def map[A, B, C] (list: List[A], func1: A => B, func2: B => C) : List[C] = {
    list.map(func1 andThen func2)
  }


  // isSorted
  def isSorted [A](list: List[A], ordered: (A,A) => Boolean) : Boolean = {
    if (list.size == 1)
      return true
    //first and second element
    if (ordered(list.head, list.tail.head)) {
      //sent to isSorted (the list without the first element, and ordered function)
      isSorted(list.tail, ordered)
    } else
      false
  }


  //val xs = Array(14,14,1,2) // values
  //val ps/probs  = Array(0.5, 0.5 ,0.25 ,0.25) // probabilities
  //counts=[ 14,2 , 1,1 , 2,1 ]

  //probs
  def probs(xs: Array[Double]): Array[Double] = {
    //creates an empty mutable map that will be used to store the frequency of each element in the input array
    val counts = mutable.Map.empty[Double,Double].withDefaultValue(0);
    //iterates over each element x in input array xs, and increments the corresponding count (value of the key x) in the counts map
    xs.foreach(x => counts(x) += 1)
    // creates a new array by mapping (changing) each element x in the input array to its probability
    xs.map(x => counts(x)/xs.length)
  }


  //    probs (another option)
  //    def probs(doubleArray: Array[Double]): Array[Double] = {
  //    doubleArray.map(x => doubleArray.count(_ == x) / doubleArray.length.toDouble)
  //  }


  // entropy
  def entropy(xs: Array[Double]): Double = {
    val n = xs.length
    val counts = xs.groupBy(identity).mapValues(_.length)
    val probabilities = counts.values.map(_.toDouble / n)
    probabilities.map(p => -p * math.log(p) / math.log(2)).sum
  }

  // mu explanation
  // E(X) = sum x{i} * P (X=x{i})
  // E(X) = 1*(1/6) + 2*(1/6) + 3*(1/6) + 4*(1/6) + 5*(1/6) + 6*(1/6)

  // xs = Array(1.0, 1.0, 3.0, 4.0, 4.0)
  // E(X) = 1*(2/5) + 1*(2/5) + 3*(1/5) + 4*(2/5) + 4*(2/5) = 4.6

  //The calculation
  // probs = (2/5, 2/5 , 1/5 ,2/5, 2/5)
  // E(x) = x*probs

  //    def mu(xs: Array[Double], probs: Array[Double]): Array[Double] = {
  //    // Ensure that the lengths of xs and probs are equal
  //    // "zipping" together the elements of xs and probs
  //    require(xs.length == probs.length, "Arrays must have the same length.")
  //
  //    // Multiply each variable x in xs by its corresponding probability
  //    val updated = xs.zip(probs).map { case (x, p) => x * p }
  //
  //    // Return the updated array
  //    updated
  //  }

  // mu
  def mu(xs: Array[Double]): Double = xs.sum / xs.length

  // variance
  def variance(xs: Array[Double]): Double = {
    val m = mu(xs)
    xs.map(x => math.pow(x - m, 2)).sum / xs.length
  }


  // zscore
  // [z-score tells you how many standard deviations a particular value is from the mean]
  def zscore(xs: Array[Double], x: Double): Double = {
    //first calculates means and variance of xs
    val m = mu(xs)
    val s = math.sqrt(variance(xs))
    //calculates the z-score of x by subtracting m from x, and dividing the result by s
    (x - m) / s
  }


  // cov
  // calculates the covariance between two arrays of values xs and ys
  def cov(xs: Array[Double], ys: Array[Double]): Double = {
    val n = xs.length
    // calculates the mean
    val mx = mu(xs)
    val my = mu(ys)
    // iterate over each value in xs and ys
    // subtracts the corresponding means from the values, multiplies them together, and sums the results
    // Finally, divides the sum by the number of values in xs to get the covariance
    (0 until n).map(i => (xs(i) - mx) * (ys(i) - my)).sum / n
  }


  // pearson
  // calculates the Pearson correlation coefficient between two arrays of values xs and ys

  // [Pearson correlation coefficient measures the linear relationship between two variables and ranges from -1 to 1, with values close to -1 indicating a strong negative correlation, values close to 1 indicating a strong positive correlation, and values close to 0 indicating no correlation]

  def pearson(xs: Array[Double], ys: Array[Double]): Double = {
    val n = xs.length
    // calculates means and variance of xs and ys
    val mx = mu(xs)
    val my = mu(ys)
    val sx = math.sqrt(variance(xs))
    val sy = math.sqrt(variance(ys))
    // iterate over each value in xs and ys,
    // subtracts the corresponding means from the values,
    // multiplies them together, and sums the results.
    // Finally, divides the sum by the product of n (the number of values in xs), sx, and sy to get the correlation coefficient
    (0 until n).map(i => (xs(i) - mx) * (ys(i) - my)).sum / (n * sx * sy)
  }
}