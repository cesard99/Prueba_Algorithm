package Forest
import Repository.MyDataset
import Repository.MyInstance
import Utils.DoubleCompare
import Utils.Randomize



class ChooseInterval( dataset : MyDataset,instaceIndexes:Array[Int] , attributeIndex:Int) {
  private val this.dataset=dataset
  private val this.instaceIndexes = instaceIndexes
  private val this.attributeIndex =attributeIndex
  private val interval = new Interval(attributeIndex)


  def run():Unit={
    if(dataset.getAttribute(attributeIndex).isNominal){
      chooseIntervalNominal()
    }else{
      chooseIntervalNumeric()
    }
  }
  def chooseIntervalNumeric():Unit={
    dataset.sort(attributeIndex,instaceIndexes)

    val size = instaceIndexes.length
    val rigthClassSupp: Array[Int] = dataset.getClassSupp(instaceIndexes)
    var oldEntropy = Entropy().entropy(rigthClassSupp,size)
    val leftClassSupp = new Array[Int](rigthClassSupp.length)

    for (i <- 0 until size){
      val instance: MyInstance = dataset.getInstance(instaceIndexes(i - 1))
      val classValue = instance.classValue
      leftClassSupp(classValue)+=1
      rigthClassSupp(classValue)-=1

      var value1= instance.value(attributeIndex)
      var value2 =dataset.getInstance(instaceIndexes(i)).value(attributeIndex)

      if(!DoubleCompare().equals(value1,value2)){
        var lb : Int=0
        var ub: Int =0
        var positiveClassSupport:Array[Int]= null
        var negativeClassSupport:Array[Int]= null

        if (Randomize.randBoolean()){
          lb=0
          ub=i
          positiveClassSupport=leftClassSupp
          negativeClassSupport=rigthClassSupp
        }else{
          lb= i
          ub=size
          positiveClassSupport=rigthClassSupp
          negativeClassSupport=leftClassSupp

        }

        var positiveIntervalSize= ub-lb
        var negativeIntervalSize =size-positiveIntervalSize

        var infoGaing = oldEntropy - positiveIntervalSize*Entropy().entropy(positiveClassSupport,positiveIntervalSize)/size - negativeIntervalSize*Entropy().entropy(negativeClassSupport,negativeIntervalSize)/size

        if(infoGaing >= interval.getInfoGain){
          interval.setInfoGain(infoGaing)
          interval.setLowerBound(dataset.getInstance(instaceIndexes(lb)).value(attributeIndex))
          interval.setUpperBound(dataset.getInstance(instaceIndexes(ub-1)).value(attributeIndex))
        }

      }
    }
     var tempSubsets = Array.ofDim[Int](2, size)
    var count = new Array[Int](2)

    for (i <- instaceIndexes.indices){
      var instance : MyInstance = dataset.getInstance(i)
      var subsetIndex = interval.getInterval(instance.values(attributeIndex))

      tempSubsets (subsetIndex)(count(subsetIndex)) = i
      count(subsetIndex)+=1

    }

    val subsets :Array[Array[Int]] = Array.ofDim(2)

    for (i<- subsets.indices){
      subsets(i)= new Array[Int](count(i))
      Array.copy(tempSubsets(i),0,subsets(i),0,count(i))

    }
    interval.setSubsets(subsets)
  }


  def chooseIntervalNominal():Unit={
    var size = instaceIndexes.length
    var subsetsSize = dataset.getAttribute(attributeIndex).numValues*2

    var subsets : Array[Array[Int]] = Array.ofDim(subsetsSize,size)
    var counts :Array[Int]= new Array[Int](subsetsSize)

    var subsetsClassSupport: Array[Array[Int]]= Array.ofDim(subsetsSize,dataset.getNumClasses)
    var classSupp : Array[Int]= new Array[Int](dataset.getNumClasses)

    for (i<- instaceIndexes.indices){
      var instace :MyInstance = dataset.getInstance(i)
      var value :Int = instace.values(attributeIndex).toInt
      var subsetIndex : Int = value*2
      var classValue : Int = instace.clasValue()

      subsets(subsetIndex)(counts(subsetIndex)+ 1)=i
      classSupp(classValue)+=1

      for (j <- 0 until subsetsSize){
        if(j/2 != value){
          subsets(j)(counts(j+1)) = i
          subsetsClassSupport(j)(classValue)+=1
        }
      }


    }

    var oldEntropy = Entropy().entropy(classSupp,size)

    for (i <- 1 until subsetsSize by 2){
      var positiveSubset :Array[Int]= new Array[Int](counts(i-1))
      Array.copy(subsets(i-1),0,positiveSubset,0,counts(i-1))

      var negativeSubset : Array[Int]= new Array[Int](counts(i))
      Array.copy(subsets(i),0,negativeSubset,0,counts(i))

      if(positiveSubset.length !=0 && negativeSubset.length !=0){
        var positiveChildrenEntropy: Double = Entropy().entropy(subsetsClassSupport(i - 1), positiveSubset.length)
        var negativeChildEntropy: Double = Entropy().entropy(subsetsClassSupport(i), negativeSubset.length)

        var avgEntropyChildren:Double = (positiveSubset.length * positiveChildrenEntropy / size ) + (negativeSubset.length * negativeChildEntropy /size)

        var infoGain :Double = oldEntropy- avgEntropyChildren

        if(infoGain>= interval.getInfoGain){
          var value = dataset.getInstance(positiveSubset(0)).value(attributeIndex)

          interval.setInfoGain(infoGain)
          interval.setLowerBound(value)
          interval.setUpperBound(value)
          interval.setSubsets(Array(positiveSubset,negativeSubset))
        }
      }

    }
  }


  def getInterval:Interval =interval

}
