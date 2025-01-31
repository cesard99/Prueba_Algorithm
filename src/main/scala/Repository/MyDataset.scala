package Repository

import java.util.Comparator
import Utils.DoubleCompare
import Utils.Util
import dataset.InstanceSet


class MyDataset {
  protected var instanceset: InstanceSet
  private var attributes: Array[MyAttribute]=_
  private var instances : Array[MyInstance]=_
  private var nClasses:Int=0
  private var nInputs:Int=0
  private var nAttr: Int=0
  private var classSupp:Array[Int]=_

  def MyDataset(dataset:MyDataset): Unit = {
    this(dataset,dataset.getNumInstances)
    dataset.copyInstance(this)
  }

  def this(dataset:MyDataset,capacity:Int) = {
    this()
    instanceset=dataset.instanceset
    nClasses=dataset.nClasses
    nInputs=dataset.nInputs
    nAttr=dataset.nAttr
    attributes=dataset.attributes
    classSupp=Array.ofDim(nClasses)
    instances=Array.ofDim(capacity)

  }
  def this(datasetFile :String,isTrain: Boolean) = {
    this()
    readSet(datasetFile,isTrain)
    readAttributes()
    readInstances()
  }

  private def readSet(datasetFIle:String, isTrain : Boolean)={
    if (isTrain)
      Attributes.clearAll()
    try {
      instanceSet = new InstanceSet()
      instanceSet.readSet(file, isTrain)
    } catch {
      case e: Exception =>
        System.exit(e.hashCode())
    }

  }

  private def readAttributes(): Unit = {
    nInputs = Attributes.getInputNumAttributes()
    nAttr = nInputs + Attributes.getOutputNumAttributes()

    attributes=Array.ofDim(nAttr)

    for (i <- 0 until nInputs) {
      val attr: Attributes = Attributes.getInputAttribute(i)
      var newAttr: MyAttribute = null

      if (attr.getType == Attribute.NOMINAL) {

        newAttr = new MyAttribute(attr.getName, attr.getNominalValuesList.asScala.toList, i)
        newAttr.setRange(0, newAttr.numValues - 1.0f)
      } else {

        newAttr = new MyAttribute(attr.getName, attr.getType, i)
        newAttr.setRange(attr.getMinAttribute, attr.getMaxAttribute)
      }
      attributes:+ newAttr
    }
  }

  private def readInstances(): Unit = {
    val numInstances = instanceSet.getNumInstances()

    val instances = new Array[MyInstance](numInstances)
    val classSupp = new Array[Int](nClasses)

    var classValue: Int = 0
    var values: Array[Double] =null

    for (j <- 0 until numInstances) {
      values = new Array[Double](getNumAttributes)

      for (i <- 0 until nInputs)
        values(i) = instanceSet.getInputNumericValue(j, i)

      classValue = instanceSet.getOutputNumericValue(j, 0).toInt

      addInstance(new MyInstance(values, classValue))
    }
    }

  private def copyInstance(dest: MyDataset)= {
    dest.instances.foreach(addInstance)
  }

  def addInstance(inst : MyInstance)={
    instances :+ inst
    classSupp(inst.clasValue())+=1
  }

  def getAttribute(index:Int):MyAttribute={
    attributes.apply(index)
  }

  def  getAttributes: Array[MyAttribute]= this.attributes
  def getClassIndex : Int =nInputs

  def getClassesSupp(pos: Int): Int = classSupp(pos)
  def getClasesSupp : Array[Int]=classSupp
  def getInstaceSet : InstanceSet =instanceSet
  def getInstance(pos: Int ):MyInstance=instances(pos)
  def getInstances:Array[MyInstance]=instances
  def isMissing(i:Int,v: Int):Boolean=  getInstance(i).isMissing(v)
  def getNumClasses:Int=nClasses
  def getNumInstances:Int= instances.length
  def getNumAttributes:Int=nAttr
  def getNumInputs: Int =nInputs
  def getClassAttribute:MyAttribute=attributes(nInputs)
  def trimToSize:Unit= instances.toList
  def sort(implicit ord: Ordering[MyInstance]): Unit = {
    instances = instances.sorted
  }

  def sort(attr: Int, instanceIndexes: Array[Int]): Unit = {
    quickSort(attr, instanceIndexes, 0, instanceIndexes.length - 1)
  }

  private def quickSort(attr: Int, instanceIndexes: Array[Int], left: Int, right: Int): Unit = {
    val pivot = instances(instanceIndexes(left)).value(attr)
    var i = left
    var j = right

    while (i < j) {
      while (!DoubleCompare().greater(instances(instanceIndexes(i)).value(attr), pivot) && i < j) {
        i += 1
      }

      while (DoubleCompare().greater(instances(instanceIndexes(j)).value(attr), pivot)) {
        j -= 1
      }

      if (i < j) {
        Util.swap(instanceIndexes, i, j)
      }
    }

    Util.swap(instanceIndexes, left, j)

    if (left < j - 1) {
      quickSort(attr, instanceIndexes, left, j - 1)
    }

    if (j + 1 < right) {
      quickSort(attr, instanceIndexes, j + 1, right)
    }
  }

  def getClassSupp(instanceIndexes: Array[Int]): Array[Int] = {
    val classSupport = Array.fill(nClasses)(0)

    instanceIndexes.foreach(i => classSupport(instances(i).classValue) += 1)

    classSupport
  }


}
