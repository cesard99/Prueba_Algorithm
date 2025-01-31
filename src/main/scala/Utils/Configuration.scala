package Utils

import Utils.{ConfigurationParser, Randomize}



class Configuration {
  private  var name : String =_
  private var inputFileTra: String =_
  private var inputFileTst: String =_
  private var rulesFile: String =_
  private var durationFile:String =_
  private var measureFileTra : String =_
 // private var covFileTra :String =_
 // private var evolveMetricsFile:String =_
  private var populationBuilderTy : String =_
  private var maxTrials: Int =0
  private var numObjetives : Int=0
  private var h:Int =0
  private var t: Int=0
  private var deltaProbability : Double =0
  private var numRepetitions :Int =0
  private var mutationProbabilty:Double =0.0
  private var amplitudeFactor : Double= 0.0
  private var percentUpdate: Double=0.0
  private var redundancyThreshold : Double =0.0
  private var dominanceThreshold : Double =0.0
  private var minInstances: Double =0.0
  private var csvDelimiter:String =_

  def this(p: ConfigurationParser)= {
    this()
    name =p.algorithmName

    inputFileTra= p.getInputFile(1)
    inputFileTst=p.getInputFile(2)

    rulesFile= p.getOutputFile(0)
    durationFile=p.getOutputFile(1)
    measureFileTra=p.getOutputFile(2)
   // covFileTra=p.getOutputFile(3)
   // evolveMetricsFile=p.getOutputFile(4)


    Randomize.setSeed(p.getParameter(0).toLong)
    populationBuilderTy=p.getParameter(1)
    numObjetives=p.getParameter(2).toInt
    maxTrials=p.getParameter(3).toInt
    h=p.getParameter(4).toInt
    t=p.getParameter(5).toInt
    deltaProbability=p.getParameter(6).toDouble
    numRepetitions=p.getParameter(7).toInt
    mutationProbabilty=p.getParameter(8).toDouble
    amplitudeFactor=p.getParameter(9).toDouble
    percentUpdate=p.getParameter(10).toDouble
    redundancyThreshold=p.getParameter(11).toDouble
    dominanceThreshold=p.getParameter(12).toDouble
    minInstances=p.getParameter(13).toDouble
    csvDelimiter=p.getParameter(14)

  }

  def getName:String =name
  def getInputFileTra:String=inputFileTra
  def getInputFileTst:String =inputFileTst
  def getRulesFile:String =rulesFile
  def getDurationFile:String =durationFile
  def getMeasureFileTra:String =measureFileTra
  //def getCovFileTra :String =covFileTra
  //def getEvolveMetricsFile:String =evolveMetricsFile
  def getPopulationBuilderType:String =populationBuilderTy

  def getMaxTrials: Int = maxTrials
  def getNumObjetives: Int = numObjetives
  def getH:Int =h
  def getT:Int =t
  def getDeltaProbability:Double=deltaProbability
  def getNumRepetitions:Int=numRepetitions
  def getMutationProbability:Double =mutationProbabilty
  def getAmplitudeFactor :Double=amplitudeFactor
  def getPercentUpdate: Double=percentUpdate
  def getRedundancyThreshold:Double=redundancyThreshold
  def getDominanceThreshold:Double= dominanceThreshold
  def getMinInstances : Double= minInstances
  def getCsvDelimiter:String=csvDelimiter
}
