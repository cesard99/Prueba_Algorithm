package Population

import Codification.Individual

import scala.collection.mutable.ArrayBuffer

class Population(val size : Int, val z : Array[Double],val zMin : Array[Double]) extends Cloneable{
  protected var individuals :ArrayBuffer[Individual] = new ArrayBuffer[Individual](size)


  def noneEqualIndividual(ind: Individual): Boolean = {
    //En scala se usa ForAll para verificar que todos los elementos cumplan una condicion
    individuals.forall(individual => !ind.equals(individual))
  }

  def getIndividuals: ArrayBuffer[Individual]= individuals
  def getIndividual(index: Int): Individual= individuals.apply(index)
  def getZ :Array[Double]=z
  def sizze:Int= individuals.length
  def setIndividual(index : Int , individual: Individual): Unit =individuals.update(index,individual)
  def clear():Unit= individuals.clear()
  def addIndividual(individual: Individual)=individuals +=individual

  override def toString: String = {
    val str = new StringBuilder
    for (i <- individuals) {
      str.append(i.getChromosome).append("\n")
    }
    str.toString()
  }

  def metricsSum(): Array[Double] = {
    val metrics = new Array[Double](3)

    individuals.foreach { individual =>
      metrics(0) = metrics(0) + individual.getMetrics.getConfidence
      metrics(1) = metrics(1) + individual.getMetrics.getWracc
      metrics(2) = metrics(2) + individual.getMetrics.getSensitivity
    }

    metrics
  }

}
