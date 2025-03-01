package Forest

import Repository.{MyAttribute, MyDataset, MyInstance}
import Utils.DoubleCompare

import java.util.Random



class ChooseInterval( private val dataset : MyDataset,private  val instaceIndexes:Array[Int] , private val attribute: MyAttribute) {
  private val interval = new Interval(attribute)

  def run():Unit={
    if(attribute.isNominal)chooseIntervalNominal() else chooseIntervalNumeric()
  }

  // Metodo para buscar si el intervalo es que se requiere es Numerico
  private def chooseIntervalNumeric():Unit={
    val attr = attribute.getIndex
    dataset.sort(attr,instaceIndexes)
    val size = instaceIndexes.length
    val rigthClassSupp: Array[Int] = dataset.getClassSupp(instaceIndexes)
    val oldEntropy = Entropy().entropy(rigthClassSupp,size)
    val leftClassSupp = new Array[Int](rigthClassSupp.length)

    for (i <- 1 until size){
      val instance: MyInstance = dataset.getInstance(instaceIndexes(i - 1))
      val classValue = instance.classValue
      leftClassSupp(classValue)+=1
      rigthClassSupp(classValue)-=1

      val value1= instance.value(attr)
      val value2 =dataset.getInstance(instaceIndexes(i)).value(attr)

      if(!DoubleCompare().equals(value1,value2))
        if(Random().nextBoolean())
          computeInfoGain(oldEntropy,0,i,leftClassSupp,rigthClassSupp)
        else
          computeInfoGain(oldEntropy,i,size,rigthClassSupp,leftClassSupp)
    }
    buildInterval()
  }

  private def chooseIntervalNominal():Unit={
    val attrIndex= attribute.getIndex
    val size =instaceIndexes.length
    val subsetsSize = attribute.numValues*2

    val subsets = Array.ofDim[Int](subsetsSize,size)
    val counts:Array[Int]= new Array[Int](subsetsSize)

    val subsetsClassSupport = Array.ofDim[Int](subsetsSize,dataset.getNumClasses)
    val classSupp :Array[Int]= new Array[Int](dataset.getNumClasses)

    for (i <- instaceIndexes){
      val instance : MyInstance = dataset.getInstance(i)
      val value = instance.values(attrIndex).toInt
      val subsetIndex = value *2
      val classValue=instance.classValue

      subsets(subsetIndex)(counts(subsetIndex)+1)=i
      subsetsClassSupport(subsetIndex)(classValue)+=1
      classSupp(classValue)+=1

      for(j<- 1 until subsetsSize by 2){
        if(j/2 != value){
          subsets(j)(counts(j)+1)= i
          subsetsClassSupport(j)(classValue)+=1
        }
      }
    }
    val oldEntropy : Double= Entropy().entropy(classSupp,size)

    for(i <- 1 until subsetsSize by 2){
      val positiveSubset: Array[Int]= new Array[Int](counts(i-1))
      Array.copy(subsets(i-1),0,positiveSubset,0,counts(i-1))

      val negativeSubset : Array[Int]= new Array[Int](counts(i))
      Array.copy(subsets(i),0,negativeSubset,0,counts(i))

      if(positiveSubset.length != 0 && negativeSubset.length !=0)
        computeInfoGain(oldEntropy,size,positiveSubset,subsetsClassSupport(i-1),negativeSubset,subsetsClassSupport(i))
    }
  }

  private def computeInfoGain(oldEntropy:Double, lb:Int,ub:Int,positiveClassSupport:Array[Int], negativeClassSupport:Array[Int]):Unit={
    val attr: Int = attribute.getIndex
    val size: Int = dataset.getNumInstances

    val positiveIntervalSize = ub - lb
    val negativeIntervalSize = size - positiveIntervalSize

    val infoGain = oldEntropy -
      positiveIntervalSize*Entropy().entropy(positiveClassSupport,positiveIntervalSize)/size -
      negativeIntervalSize* Entropy().entropy(negativeClassSupport,negativeIntervalSize)/size

    if(infoGain>= interval.getInfoGain){
      interval.setInfoGain(infoGain)
      interval.setLowerBound(dataset.getInstance(instaceIndexes(lb)).values(attr))
      interval.setUpperBound(dataset.getInstance(instaceIndexes(ub-1)).values(attr))
    }
  }

  private def computeInfoGain(oldEntropy : Double, size:Int,positiveSubset:Array[Int],positiveClassSupp:Array[Int],negativeSubset:Array[Int],negativeClassSupp:Array[Int]):Unit={
    val attrIndex = attribute.getIndex

    val infoGain = oldEntropy -
      positiveSubset.length * Entropy().entropy(positiveClassSupp,positiveSubset.length)/size -
      negativeSubset.length * Entropy().entropy(negativeClassSupp,negativeSubset.length)/size

    if(infoGain >= interval.getInfoGain){
      val value : Double = dataset.getInstance(positiveSubset(0)).values(attrIndex)

      interval.setInfoGain(infoGain)
      interval.setLowerBound(value)
      interval.setUpperBound(value)
      interval.setSubsets(Array(positiveSubset,negativeSubset))

    }
  }
  private def buildInterval():Unit={
    val size :Int= instaceIndexes.length
    val tempSubsets = Array.ofDim[Int](2,size)
    val counts : Array[Int]= new Array[Int](2)

    val attrIndex = attribute.getIndex

    for(i<- instaceIndexes){
      val instance : MyInstance = dataset.getInstance(i)
      val subsetIndex = interval.getInterval(instance.values(attrIndex))
      tempSubsets(subsetIndex)(counts(subsetIndex))=i
      counts(subsetIndex)+=1
    }
    val subsets = Array.ofDim[Int](2,0)
    for (i <- subsets.indices){
      subsets(i)= new Array[Int](counts(i))
      Array.copy(tempSubsets(i),0,subsets(i),0, counts(i))
    }
    interval.setSubsets(subsets)
  }

  def getInterval:Interval =interval

}
