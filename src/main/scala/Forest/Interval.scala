package Forest
import Repository.MyAttribute
import Utils.DoubleCompare

class Interval(private val attribute: MyAttribute){
  private var subsets: Array[Array[Int]] = Array.empty
  private var lowerBound: Double= -1.0
  private var upperBound: Double= -1.0
  private var infoGain: Double= -1.0

 
  def getInterval(value: Double):Int= if (DoubleCompare().greaterEquals(value,lowerBound) && DoubleCompare().lessEquals(value,upperBound)) 0 else 1
  
  def getAttribute:MyAttribute=attribute
  def getSubsets:Array[Array[Int]]=subsets
  def setSubsets(subsets : Array[Array[Int]]): Unit =this.subsets=subsets
  def getLowerBound:Double=lowerBound
  def getupperBound:Double=upperBound
  def setLowerBound(lowerbound: Double): Unit= this.lowerBound=lowerbound
  def setUpperBound( upperBound : Double):Unit= this.upperBound=upperBound
  def getInfoGain:Double=infoGain
  def setInfoGain(infoGain: Double):Unit=this.infoGain=infoGain
}
