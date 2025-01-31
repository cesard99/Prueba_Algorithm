package Utils

class DoubleCompare {
  private final val EPSILON = 0.00001

  def equals(a: Double, b: Double): Boolean = math.abs(a - b) < EPSILON
  def greater(a: Double, b: Double): Boolean = a - b > EPSILON
  def less(a: Double, b: Double): Boolean = b - a > EPSILON
  def greaterEquals(a: Double, b: Double): Boolean = !less(a, b)
  def lessEquals(a: Double, b: Double): Boolean = !greater(a, b)
}
