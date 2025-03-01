package Population
import Codification.Individual
import Utils.DoubleCompare

class UpdatedPopulation(size: Int, z: Array[Double], zMin: Array[Double]) extends Population(size, z, zMin){

  def initializeZ(): Unit = {
    for (i <- z.indices) {
      z(i) = bestObjectiveValue(i)
      zMin(i) = if (i == 1) -1.0f else 0.0f
    }
  }

  private def bestObjectiveValue(obj: Int): Double = {
    var best = individuals(0).getMetrics.getObjective(obj)

    for (i <- 1 until individuals.size if best != 1) {
      val curr = individuals(i).getMetrics.getObjective(obj)
      if (DoubleCompare().greater(curr, best)) best = curr
    }

    best
  }

  def updateZ(ind: Individual): Unit = {
    for (i <- z.indices) {
      val obj = ind.getMetrics.getObjective(i)

      if (DoubleCompare().less(z(i), obj)) z(i) = obj
      if (DoubleCompare().greater(zMin(i), obj)) zMin(i) = obj
    }
  }

  def tchebycheffApproach(ind: Individual): Double = {
    var gMax = ind.getWeight(0) * (math.abs(z(0) - ind.getObjective(0)) / (z(0) - zMin(0)))

    for (i <- 1 until z.length) {
      val values: Double = ind.getWeight(i) * (math.abs(z(i) - ind.getObjective(i)) / (z(i) - zMin(i)))
      if (DoubleCompare().greater(values, gMax)) gMax = values
    }

    gMax
  }
  
}
