package Forest
import Repository.MyDataset
import  Utils.DoubleCompare

class TreeBuilder(private val dataset: MyDataset, private val instanceIndexes: Array[Int], private val minInstances: Int) {

  def build : Node ={
    recursiveBuild(instanceIndexes, new Array[Boolean](dataset.getNumInputs),-1,-1)
  }

  private def recursiveBuild(instances :Array[Int],attributes: Array[Boolean],parentClass : Int, parentWracc : Double ):Node ={
    var chooseAttribute : ChooseAttribute = new ChooseAttribute(dataset,instances,attributes)
    chooseAttribute.run()

    var interval : Interval =chooseAttribute.getInterval
    if (interval == null){
      new Node()
    }
    var root: Node = new Node(interval)

    attributes(interval.getAttributeIndex)= true

    for(i <- 0 until interval.getNumSubset){
      root.addChild(i,buildChild(interval.getSubset(i),attributes,parentClass,parentWracc))

    }
    attributes(interval.getAttributeIndex)=false
    root

  }

  private def buildChild(instances :Array[Int],attributes:Array[Boolean],parentClass: Int , parentWracc : Double):Node={
    if (instances.length > minInstances){
      var majorityClass : Array[Int]=computeMajorityClass(instances)

      if(majorityClass(1) != instances.length){
        var wracc: Double= computeWracc(instances,majorityClass(0),majorityClass(1))

        if(majorityClass(0) != parentClass || DoubleCompare().greaterEquals(wracc,parentWracc))
          return recursiveBuild(instances,attributes,majorityClass(0),wracc)
      }

    }
    new Node()
  }

  private def computeMajorityClass(instaces : Array[Int]):Array[Int]={
    var classSupp : Array[Int]= dataset.getClassSupp(instaces)

    var majorityClass: Int =0
    var majorityClassSupport: Int = classSupp(0)

    for (j<-1 until classSupp.length){
      if(classSupp(j)>majorityClassSupport){
        majorityClass=j
        majorityClassSupport=classSupp(j)
      }

    }

    Array(majorityClass,majorityClassSupport)
  }

  private def computeWracc(instances :Array[Int],majorityClass :Int , majorityClassSupport: Int):Double={

    var fp :Int = instances.length- majorityClassSupport
    var tn : Int = dataset.getClassesSupp(majorityClass)-majorityClassSupport
    var fn : Int = dataset.getNumInstances - instances.length - tn

    var total : Double = dataset.getNumInstances

    (majorityClassSupport/total)-((majorityClassSupport+fp)/total)* ((majorityClassSupport + fn )/total)
  }
}
