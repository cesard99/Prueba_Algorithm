package Codification

import Repository.{MyAttribute, MyDataset, MyInstance}
import Utils.DoubleCompare
import Utils.Randomize

import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale

class Chromosome(  var dataset : MyDataset) extends  Cloneable {
  var numGenes: Int = dataset.getNumInputs

  private var lowerBound: Array[Double] = new Array[Double](numGenes)
  private var upperBound: Array[Double] = new Array[Double](numGenes)
  private var positiveIntervals: Array[Boolean] = new Array[Boolean](numGenes)
  private var involvedGenes: Array[Boolean] = new Array[Boolean](numGenes)
  private var numInvolvedGenes: Int = _

  def forceConsistency(): Unit = {
    if (numInvolvedGenes == 0) {
      var numGenes: Int = dataset.getNumInputs

      var i = Randomize.randInt(0, numGenes)
      while (!involvedGenes(i)) {
        setInvolved(true, i)
        i = Randomize.randInt(0, numGenes)
      }
    }

  }

  def isCovered(instance: MyInstance): Boolean = {
    for (i <- 0 until dataset.getNumInputs) {
      if (involvedGenes(i) && (!isCovered(instance, i) || instance.isMissing(i)))
        return false
    }
     true
  }

  def isCovered(ins: MyInstance, index: Int): Boolean = {

    var isPositiveInterval: Boolean = positiveIntervals(index)
    var value: Double = ins.value(index)
    var lb = lowerBound(index)

    if (dataset.getAttribute(index).isNominal) {
      return isPositiveInterval == DoubleCompare().equals(value, lb)
    }
    if (isPositiveInterval) {
      return DoubleCompare().greaterEquals(value, lb)
    }
     DoubleCompare().less(value, lb) || DoubleCompare().greater(value, upperBound(index))

  }

  def equals(chr: Chromosome): Boolean = {
    var numGenes: Int = dataset.getNumInputs

    for (i <- 0 until numGenes) {
      if (involvedGenes(i) != chr.involvedGenes(i) || positiveIntervals(i) != chr.positiveIntervals(i) || !DoubleCompare().equals(lowerBound(i), chr.lowerBound(i)) || !DoubleCompare().equals(upperBound(i), chr.upperBound(i))) {
        return false
      }
    }
     true
  }

  override def clone(): Chromosome = {
    val cloned = super.clone().asInstanceOf[Chromosome]
    cloned.dataset = this.dataset
    cloned.lowerBound = this.lowerBound.clone()
    cloned.upperBound = this.upperBound.clone()
    cloned.positiveIntervals = this.positiveIntervals.clone()
    cloned.involvedGenes = this.involvedGenes.clone()
    cloned
  }

  def getNumInvolvedGenes: Int = numInvolvedGenes

  def setLowerBound(value: Double, index: Int): Unit = {
    lowerBound(index) = value
  }

  def setUpperBound(value: Double, index: Int): Unit = {
    upperBound(index) = value
  }

  def getLowerBound(index: Int): Double = lowerBound(index)

  def getUpperBound(index: Int): Double = upperBound(index)

  def isPositiveInterval(index: Int): Boolean = positiveIntervals(index)

  def setPositiveInterval(value: Boolean, index: Int): Unit = positiveIntervals(index) = value

  def isInvolved(index: Int): Boolean = involvedGenes(index)

  def setInvolved(value: Boolean, index: Int): Unit = {
    if (involvedGenes(index) != value) {
      numInvolvedGenes += (if (value) 1 else -1)
    }
    involvedGenes(index) = value
  }

  def cloneGene(chromosome: Chromosome, index: Int): Unit = {
    lowerBound(index) = chromosome.lowerBound(index)
    upperBound(index) = chromosome.upperBound(index)
    positiveIntervals(index) = chromosome.positiveIntervals(index)
    setInvolved(chromosome.involvedGenes(index), index)
  }

  
  override def toString:String={
    val locale : Locale = new Locale("en","UK")
    var pattern : String = "###.####"

    val df = NumberFormat.getNumberInstance(locale).asInstanceOf[DecimalFormat]
    df.applyPattern(pattern)

    var contents: StringBuilder = new StringBuilder("\"")
    var size = dataset.getNumInputs

    for (i <- 0 until size){
      if (involvedGenes(i)){
        if(contents.length != 1) {
          contents.append(" AND")
        }
        var a : MyAttribute = dataset.getAttribute(i)
        contents.append(a.name).append("")

        if(a.isNominal){
          contents.append(if (positiveIntervals(i)) "" else "NOT ").append(a.value(upperBound(i).toInt))
        }else{
          contents.append(if (positiveIntervals(i)) "[" else "NOT [").append(lowerBound(i)).append(", ").append(upperBound(i)).append("]")

        }
      }
    }

    contents.append("-->").append(dataset.getClassAttribute.name).toString()
  }


}

