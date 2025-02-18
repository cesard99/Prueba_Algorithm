package Forest
import Repository.MyDataset
import  Utils.DoubleCompare

class TreeBuilder(private val dataset: MyDataset, private val instanceIndexes: Array[Int], private val minInstances: Int) {

  def build : Node = recursiveBuild(instanceIndexes, new Array[Boolean](dataset.getNumInputs))


  private def recursiveBuild(instances :Array[Int],attributesUsed: Array[Boolean]):Node ={
    val chooseAttribute : ChooseAttribute = new ChooseAttribute(dataset,instances,attributesUsed)
    chooseAttribute.run()

    val interval : Interval =chooseAttribute.getInterval
    if (interval == null) return new Node()
    
    val root: Node = new Node(interval)
    val attrIndex:Int = interval.getAttribute.getIndex
    attributesUsed(attrIndex)=true
    
    val subsets : Array[Array[Int]]= interval.getSubsets

    for(i <- subsets.indices){
      val subset:Array[Int]=subsets(i)
      val subsetSize:Int=subset.length
      
      if(subsetSize<= minInstances || getMajorityClass(subset)== subsetSize)
        root.addChild(i,new Node())
      else
       root.addChild(i,recursiveBuild(subset,attributesUsed))  
    }
    attributesUsed(attrIndex)=false
    root

  }
  def getMajorityClass(intances: Array[Int]):Int={
    val classSupp :Array[Int]= dataset.getClassSupp(intances)
    var max = classSupp(0)
    for(i<- 1 until classSupp.length ){
      if(classSupp(i)> max) max = classSupp(i)
    }
    max
  }

  
}
