package Forest
import Utils.Util

class Entropy {
  def entropy(classSupport : Array[Int],total: Int): Double= {
    var entropy: Double=0

    for (c <- classSupport) {
      val prob: Double = c / total

      if (prob != 0)
        entropy -= prob * Util.log2(prob)
    }
    entropy
  }
}