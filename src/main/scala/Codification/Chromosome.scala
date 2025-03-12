package Codification



import Repository.{MyAttribute, MyDataset, MyInstance}
import Utils.DoubleCompare

import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale
import scala.util.Random

class Chromosome( private var dataset : MyDataset) extends  Cloneable {
  var numGenes: Int = dataset.getNumInputs

  private var lowerBound: Array[Double] = new Array[Double](numGenes)
  private var upperBound: Array[Double] = new Array[Double](numGenes)
  private var positiveIntervals: Array[Boolean] = new Array[Boolean](numGenes)
  private var usedGenes: Array[Boolean] = new Array[Boolean](numGenes)
  private var numUsedGenes: Int = 0

  def forceConsistency(): Unit = {
    if(numUsedGenes == 0){
      var i= Random.nextInt(numGenes)
      while (!usedGenes(i)){
        setUsedGene(true,i)
        i = Random.nextInt(numGenes)
      }

    }
  }

  def isCovered(instance: MyInstance): Boolean = {
    val numGenes= dataset.getNumInputs
    var flag = true
    for(i<- 0 until numGenes if flag){
      if (usedGenes(i) && (!isCovered(instance, i) || instance.isMissing(i)))
        flag = false
    }
     flag
  }

  def isCovered(ins: MyInstance, index: Int): Boolean = {

    val isPositiveInterval: Boolean = positiveIntervals(index)
    val value: Double = ins.value(index)
    val lb = lowerBound(index)

    if (dataset.getAttribute(index).isNominal) {
      return isPositiveInterval == DoubleCompare().equals(value, lb)
    }
    if (isPositiveInterval) {
      return DoubleCompare().greaterEquals(value, lb)
    }
     DoubleCompare().less(value, lb) || DoubleCompare().greater(value, upperBound(index))

  }

  def equals(chr: Chromosome): Boolean = {
    val numGenes: Int = dataset.getNumInputs
    var flag :Boolean= true

    for(i<-0 until numGenes if flag){
      if (usedGenes(i) != chr.usedGenes(i) || positiveIntervals(i) != chr.positiveIntervals(i) || !DoubleCompare().equals(lowerBound(i), chr.lowerBound(i)) || !DoubleCompare().equals(upperBound(i), chr.upperBound(i)))
        flag = false
    }
     flag
  }

  override def clone(): Chromosome = {
    val cloned = super.clone().asInstanceOf[Chromosome]
    cloned.dataset = this.dataset
    cloned.lowerBound = this.lowerBound.clone()
    cloned.upperBound = this.upperBound.clone()
    cloned.positiveIntervals = this.positiveIntervals.clone()
    cloned.usedGenes = this.usedGenes.clone()
    cloned
  }

  def getNumUsedGenes: Int = numUsedGenes

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

  def isUsedGene(index: Int): Boolean = usedGenes(index)

  def setUsedGene(value: Boolean, index: Int): Unit = {
    if (usedGenes(index) != value) {
      numUsedGenes += (if (value) 1 else -1)
    }
    usedGenes(index) = value
  }

  def cloneGene(chromosome: Chromosome, index: Int): Unit = {
    lowerBound(index) = chromosome.lowerBound(index)
    upperBound(index) = chromosome.upperBound(index)
    positiveIntervals(index) = chromosome.positiveIntervals(index)
    setUsedGene(chromosome.usedGenes(index), index)
  }

  
  override def toString:String={
    val locale : Locale = new Locale("en","UK")
    val pattern: String = "###.####"

    val df = NumberFormat.getNumberInstance(locale).asInstanceOf[DecimalFormat]
    df.applyPattern(pattern)

    val contents: StringBuilder = new StringBuilder("\"")
    val size = dataset.getNumInputs

    for (i <- 0 until size){
      if (usedGenes(i)){
        if(contents.length != 1) {
          contents.append(" AND")
        }
        val a: MyAttribute = dataset.getAttribute(i)
        contents.append(a.getname()).append("")

        if(a.isNominal){
          contents.append(if (positiveIntervals(i)) "" else "NOT ").append(a.value(upperBound(i).toInt))
        }else{
          contents.append(if (positiveIntervals(i)) "[" else "NOT [").append(lowerBound(i)).append(", ").append(upperBound(i)).append("]")

        }
      }
    }

    contents.append("-->").append(dataset.getClassAttribute.getname()).toString()
  }


}

