package Codification
import Utils.{Configuration, DoubleCompare}
import Repository.{MyDataset, MyInstance}

import java.lang.Comparable
import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale

class Individual(private var conf : Configuration, private var dataset: MyDataset, private var chromosome: Chromosome)  extends Comparable[Individual],Cloneable{
  private var metrics: Metrics=_
  private var winClass:Int=0

  def computeObjetives():Unit={

    val numInstances: Int = dataset.getNumInstances
    val perClassAntSup: Array[Int] = new Array[Int](dataset.getNumClasses)
    val tempCovered: Array[Int] = new Array[Int](numInstances)
    var antSup: Int =0
    for(i<- 0 until numInstances){
      val instance: MyInstance = dataset.getInstance(i)
      if(chromosome.isCovered(instance)){
         perClassAntSup(instance.clasValue())+=1
         tempCovered(antSup)= i
         antSup+=1
      }

    }
    val covered: Array[Int] = new Array[Int](antSup)
    Array.copy(tempCovered,0,covered,0,antSup)

    computeWinClass(perClassAntSup,antSup)
    computeMetrics(perClassAntSup,antSup,covered)
  }

  private def computeWinClass(perClassAntSup: Array[Int], antSup: Int):Unit={
    var wincClassSup : Double =0
    val numClass = perClassAntSup.length

    for (k<-0 until numClass){
      val currentClassSup: Double = if (antSup == 0) {
        dataset.getClassesSupp(k)
      } else {
        perClassAntSup(k) / dataset.getClassesSupp(k).toDouble
      }
      if(DoubleCompare().greaterEquals(currentClassSup,wincClassSup)){
        wincClassSup=currentClassSup
        winClass=k

      }

    }
  }

  private def computeMetrics(perClassSup : Array[Int], antSup : Int, covered  : Array[Int]):Unit={
    val tp: Int = perClassSup(winClass)
    val fp: Int = antSup - tp
    val fn: Int = dataset.getClassesSupp(winClass) - tp
    val tn: Int = dataset.getNumInstances - tp - fp - fn

    metrics= new Metrics(conf, new ConfusionMatrix(tp,tn,fp,fn))
    metrics.setCovered(covered)
    metrics.compute(chromosome.getNumUsedGenes)
  }

  def redundant(individual: Individual): Boolean = {
    metrics.redundant(individual.metrics)
  }

  def dominance(individual: Individual): Int = {
    metrics.dominance(individual.metrics)
  }

  override def compareTo(individual: Individual): Int = {
    -java.lang.Double.compare(metrics.getWracc, individual.metrics.getWracc)
  }

  def equals(individual: Individual): Boolean = {
    chromosome.equals(individual.chromosome)
  }

  override def clone : Individual={
    var clone : Individual = null
    try {
      clone = super.clone().asInstanceOf[Individual]
      clone.conf =this.conf
      clone.dataset = dataset
      clone.chromosome = chromosome.clone()
      clone.metrics = metrics.clone()
    } catch {
      case _: CloneNotSupportedException => // Es compatible
    }
    clone
  }

  def forceConsistency(): Unit = {
    chromosome.forceConsistency()
  }

  def getMetrics: Metrics = metrics

  def setMetrics(metrics: Metrics): Unit = {
    this.metrics = metrics
  }

  def getWinClass: Int = winClass

  def setWinClass(winClass: Int): Unit = {
    this.winClass = winClass
  }

  def getChromosome: Chromosome = chromosome

  def getWeight(index: Int): Double = {
    metrics.getWeight(index)
  }

  def getObjective(index: Int): Double = {
    metrics.getObjective(index)
  }

  override def toString: String = {
    val locale = new Locale("en", "UK")
    val pattern = "###.####"

    val df = NumberFormat.getNumberInstance(locale).asInstanceOf[DecimalFormat]
    df.applyPattern(pattern)

    s"${chromosome.toString} ${dataset.getClassAttribute.value(winClass)}\",\"" +
      s"${df.format(metrics.getSupport)}," +
      s"${df.format(metrics.getOddRatio)}," +
      s"${df.format(metrics.getSensitivity)}\n"
  }
  
  
}
