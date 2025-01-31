package Codification
import Utils.{Configuration, DoubleCompare, Util}

import java.text.{DecimalFormat, NumberFormat}
import java.util.Locale

class Metrics(private var conf: Configuration, private var confusionMatrix: ConfusionMatrix) extends  Cloneable{
  private var coverage: Double = 0.0
  private var certaintyFactor: Double = 0.0
  private var confidence: Double = 0.0
  private var consequentSupport: Double = 0.0
  private var conviction: Double = 0.0
  private var lift: Double = 0.0
  private var netConf: Double = 0.0
  private var sensitivity: Double = 0.0
  private var support: Double = 0.0
  var wracc: Double = 0.0
  private var yulesQ: Double = 0.0
  private var oddRatio: Double = 0.0

  private var objectives: Array[Double] = Array()
  var weights: Array[Double] = Array()
  private var neighbors: Array[Int] = Array()
  var covered: Array[Int] = Array()

  def this()=this(null,null)

  private def computeCoverage(confusionMatrix: ConfusionMatrix): Double = {
    confusionMatrix.antecedentCovered() / confusionMatrix.total().toDouble
  }

  def computeConfidence(confusionMatrix: ConfusionMatrix): Double = {
    val coverage = computeCoverage(confusionMatrix)

    if (DoubleCompare().greater(coverage, 0)) {
       getSupport(confusionMatrix) / coverage
    } else {
      0.0
    }
  }

  def computeCertaintyFactor(confusionMatrix: ConfusionMatrix): Double = {
    val consequentSupport :Double = computeConsequentSupport(confusionMatrix)
    val confidence = computeConfidence(confusionMatrix)

    val den: Double = if (confidence > consequentSupport) {
      1 - consequentSupport
    } else {
      consequentSupport
    }

    (confidence - consequentSupport) / den
  }

  def computeConsequentSupport(matrix: ConfusionMatrix):Double={
    confusionMatrix.antecedentCovered()/ confusionMatrix.total().toDouble
  }

  def computeConviction(confusionMatrix: ConfusionMatrix):Double={
    val consequentSupport: Double = computeConsequentSupport(confusionMatrix)

    if (DoubleCompare().equals(consequentSupport,1)){
      return  1
    }
    val coverage: Double = computeCoverage(confusionMatrix)
    val support: Double = getSupport(confusionMatrix)

    if(DoubleCompare().equals(coverage,support)){
       1
    } else{
      coverage * (1-consequentSupport)/(coverage-support)
    }
  }

  def computeLift(confusionMatrix: ConfusionMatrix): Double = {
    val coverage = computeCoverage(confusionMatrix)

    if (DoubleCompare.equals(coverage, 0)) {
      1
    } else {
      val consequentSupport = computeConsequentSupport(confusionMatrix)

      if (DoubleCompare.equals(consequentSupport, 0)) {
        1
      } else {
        getSupport(confusionMatrix) / (coverage * consequentSupport)
      }
    }
  }

  def computeNetConf(confusionMatrix: ConfusionMatrix): Double = {
    val coverage : Double = computeCoverage(confusionMatrix)

    if ( DoubleCompare().equals(coverage,0) || DoubleCompare().equals(coverage,1)){
       return 0
    }
    val den : Double= coverage*(1-coverage)
    if(DoubleCompare().equals(den,0)){
      0
    }else
      (getSupport(confusionMatrix)-coverage*computeConsequentSupport(confusionMatrix)) /den
  }

  def getSupport(matrix: ConfusionMatrix):Double={
    matrix.getTp / matrix.total().toDouble
  }

  def computeSensitivity(confusionMatrix: ConfusionMatrix):Double={
    getSupport(confusionMatrix)/computeConsequentSupport(confusionMatrix)
  }

  def computeYulesQ(confusionMatrix: ConfusionMatrix):Double={
    val coverage: Double= computeCoverage(confusionMatrix)

    if(DoubleCompare().equals(coverage,0) || DoubleCompare().equals(coverage,1)){
      return 0
    }
    val consequentSupport :Double= computeConsequentSupport(confusionMatrix)

    if(DoubleCompare().equals(consequentSupport,0) || DoubleCompare().equals(consequentSupport,1)){
      return 0
    }
    val support =getSupport(confusionMatrix)
    val left : Double = support*(1-consequentSupport-coverage+support)
    val rigth : Double = (coverage-support)*(consequentSupport-support)
    val den : Double= left+rigth
    if(DoubleCompare().equals(den,0))
      0
    else
      (left-rigth)/den

  }

  def computeWracc(cm: ConfusionMatrix):Double={
    getSupport(cm)-computeCoverage(cm)*computeConsequentSupport(cm)

  }

  def computeOddRatio(cm: ConfusionMatrix): Double = {
    val fn = cm.getFn + 0.5
    val fp = cm.getFp + 0.5
    val tn = cm.getTn + 0.5
    val tp = cm.getTp + 0.5

    (tn * tp) / (fn * fp)
  }

  override def clone(): Metrics = {
    var clone: Metrics = null

    try {
      clone = super.clone().asInstanceOf[Metrics]
      clone.conf = this.conf
      clone.confusionMatrix = this.confusionMatrix
      clone.objectives = this.objectives.clone()
      clone.weights = this.weights.clone()
      clone.neighbors = this.neighbors.clone()
      clone.covered = this.covered.clone()
    } catch {
      case e: CloneNotSupportedException =>
      // it's supported
    }

    clone
  }

  def compute(numAnts: Int): Unit = {
    coverage = computeCoverage(confusionMatrix)
    certaintyFactor = computeCertaintyFactor(confusionMatrix)
    confidence = computeConfidence(confusionMatrix)
    consequentSupport = computeConsequentSupport(confusionMatrix)
    conviction = computeConviction(confusionMatrix)
    lift = computeLift(confusionMatrix)
    netConf = computeNetConf(confusionMatrix)
    sensitivity = computeSensitivity(confusionMatrix)
    support = getSupport(confusionMatrix)
    wracc = computeWracc(confusionMatrix)
    yulesQ = computeYulesQ(confusionMatrix)
    oddRatio = computeOddRatio(confusionMatrix)

    objectives = new Array[Double](conf.getNumObjetives)

    if (conf.getNumObjetives > 0) objectives(0) = wracc
    if (conf.getNumObjetives > 1) objectives(1) = confidence * sensitivity
    if (conf.getNumObjetives > 2) objectives(2) = 1.0 / numAnts
  }

  def redundant(m: Metrics): Boolean = {
    val intersection : Int= intersectiom(m)
    val overlap : Double= Math.max(intersection/ confusionMatrix.antecedentCovered().toDouble, intersection/ m.confusionMatrix.antecedentCovered().toDouble)

    if(DoubleCompare().greaterEquals(overlap,conf.getRedundancyThreshold)){
      val b1 : Array[Double]=computeRedundantBands(confusionMatrix)
      val b2 :Array[Double]= computeRedundantBands(m.confusionMatrix)

      return  DoubleCompare().lessEquals(b1(0),b2(0)) && DoubleCompare().lessEquals(b2(0),b1(1)) ||
              DoubleCompare().lessEquals(b1(0),b2(1)) && DoubleCompare().lessEquals(b2(1),b1(1)) ||
              DoubleCompare().lessEquals(b1(0), b2(0)) && DoubleCompare().lessEquals(b2(1), b1(1)) ||
              DoubleCompare().lessEquals(b2(0), b1(0)) && DoubleCompare().lessEquals(b1(1), b2(1))
    }
    false
  }

  def dominance (m : Metrics):Int={
    val intersection : Int = intersectiom(m)
    val covc1 : Double= confusionMatrix.getTp + confusionMatrix.getFp
    val covc2 : Double=m.confusionMatrix.getTp + m.confusionMatrix.getFp
    val overlap : Double= Math.max(intersection/covc1,intersection/covc2)

    if (DoubleCompare().greaterEquals(overlap,conf.getDominanceThreshold)){
      val best:Int =0
      for (i<- 0 until conf.getNumObjetives){
        if(DoubleCompare().greater(objectives(i),m.objectives(i))){
          return best + 1
        } else 
          if(DoubleCompare().less(objectives(i),m.objectives(i)))
            return best - 1
      }
      best.compareTo(0)
    }
    0
  }

  private def computeRedundantBands(cm: ConfusionMatrix): Array[Double] = {
    val fn = cm.getFn + 0.5
    val fp = cm.getFp + 0.5
    val tp = cm.getTp + 0.5
    val tn = cm.getTn + 0.5

    val odd = (tp * tn) / (fn * fp)
    val w = 1.96 * Math.sqrt(1 / tp + 1 / tn + 1 / fp + 1 / fn)
    val lb = odd * Math.exp(-w)
    val ub = odd * Math.exp(w)

    Array(lb, ub)
  }

  def intersectiom(m: Metrics): Int = {
    Util.intersection(covered, m.covered)
  }

  def getConfusionMatrix: ConfusionMatrix = confusionMatrix

  def setConfusionMatrix(confusionMatrix: ConfusionMatrix): Unit = {
    this.confusionMatrix = confusionMatrix
  }

  def getCoverage: Double = coverage

  def getCertaintyFactor: Double = certaintyFactor

  def getConfidence: Double = confidence

  def getConsequentSupport: Double = consequentSupport

  def getConviction: Double = conviction

  def getLift: Double = lift

  def getNetConf: Double = netConf

  def getSensitivity: Double = sensitivity

  def getSupport: Double = support

  def getWracc: Double = wracc

  def getYulesQ: Double = yulesQ

  def getOddRatio: Double = oddRatio

  def getObjectives: Array[Double] = objectives

  def setObjectives(objectives: Array[Double]): Unit = {
    this.objectives = objectives
  }

  def getObjective(i: Int): Double = objectives(i)

  def getWeights: Array[Double] = weights

  def setWeights(weights: Array[Double]): Unit = {
    this.weights = weights
  }

  def getWeight(i: Int): Double = weights(i)

  def getNeighbors: Array[Int] = neighbors

  def setNeighbors(neighbors: Array[Int]): Unit = {
    this.neighbors = neighbors
  }

  def getNeighbor(i: Int): Int = neighbors(i)

  def getCovered: Array[Int] = covered

  def setCovered(covered: Array[Int]): Unit = {
    this.covered = covered
  }

  def getCovered(i: Int): Int = covered(i)

  override def toString: String = {
    val locale : Locale = new Locale("en", "UK")
    val pattern = "###.####"

    val df = NumberFormat.getNumberInstance(locale).asInstanceOf[DecimalFormat]
    df.applyPattern(pattern)

    s"${df.format(objectives(0))}," +
      s"${df.format(objectives(1))}," +
      s"${df.format(objectives(2))}," +
      s"${df.format(coverage)}," +
      s"${df.format(certaintyFactor)}," +
      s"${df.format(confidence)}," +
      s"${df.format(consequentSupport)}," +
      s"${df.format(conviction)}," +
      s"${df.format(lift)}," +
      s"${df.format(netConf)}," +
      s"${df.format(sensitivity)}," +
      s"${df.format(support)}," +
      s"${df.format(yulesQ)}," +
      s"${df.format(wracc)}," +
      s"${df.format(oddRatio)}\n"
  }
}

