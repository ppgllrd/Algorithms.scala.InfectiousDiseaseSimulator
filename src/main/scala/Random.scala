/********************************************************************
 * Random generator
 *
 * Pepe Gallardo, 2020
 *
 * Partly based on Java Event-Driven Simulator by
 * Robert Sedgewick and Kevin Wayne
 *******************************************************************/

protected trait RandomOps {
  protected val rnd: java.util.Random

  def uniform(n: Int): Int = {
    if (n <= 0) throw new IllegalArgumentException("uniform: n must be apositive number")
    return rnd.nextInt(n)
  }

  def uniform(min: Int, max: Int): Int = {
    if(max <= min)
      throw new IllegalArgumentException("uniform: max should be larger than min")
    return min + rnd.nextInt(max - min)
  }

  def uniform(min: Double, max: Double): Double = {
    if(max <= min)
      throw new IllegalArgumentException("uniform: max should be larger than min")
    return min + rnd.nextDouble() * (max - min)
  }

  def bernoulli(): Boolean = {
    return rnd.nextDouble() < 0.5
  }

  def bernoulli(p: Double): Boolean = {
    if (!(p >= 0.0 && p <= 1.0))
      throw new IllegalArgumentException("bernoulli: success probability should be in [0.0, 1.0]")
    return rnd.nextDouble() < p
  }

  def normal(): Double =
    rnd.nextGaussian()

  def normal(mu: Double = 0, sigma: Double = 1): Double =
    mu + sigma * rnd.nextGaussian()
}

object Random extends RandomOps {
  protected val rnd = new java.util.Random()

  def apply(seed: Int): Random =
    new Random(seed)

  def apply(seed: Long): Random =
    new Random(seed)
}

class Random(seed: Long) extends java.util.Random(seed) with RandomOps {
  protected val rnd = this

  def this(seed: Int) {
    this(seed.toLong)
  }
}
