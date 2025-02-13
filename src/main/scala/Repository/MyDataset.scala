package Repository

import java.util.Comparator
import Utils.DoubleCompare
import Utils.Util


import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using


class MyDataset {
  private var attributes: ArrayBuffer[MyAttribute]=_
  private var instances : ArrayBuffer[MyInstance]=_
  private var nClasses:Int=0
  private var nInputs:Int=0
  private var nAttr: Int=0
  private var classSupp:Array[Int]=_
  
  def this(dataset:MyDataset) = {
    this()
    nClasses=dataset.nClasses
    nInputs=dataset.nInputs
    nAttr=dataset.nAttr
    attributes=dataset.attributes
    classSupp=new Array[Int](this.nClasses)
    instances= new ArrayBuffer[MyInstance](dataset.getNumInstances)
    dataset.copyInstance(this)
  }
  def this(pathFile :String,delimiter:String) = {
    this()
    var file :ArrayBuffer[Array[String]]=readFile(pathFile,delimiter)
    readAttributes(file)
    readInstances(file)
  }

  private def readFile(pathFile:String,delimiter:String): ArrayBuffer[Array[String]]={
    Using(Source.fromFile(pathFile)){
      source => source.getLines().map(_.split(delimiter).filter(_.nonEmpty)).filter(_.nonEmpty).to(ArrayBuffer)
    }.getOrElse(ArrayBuffer.empty)
  }

  private def readAttributes(file:ArrayBuffer[Array[String]]): Unit = {
    var header : Array[String]= file.remove(0)
    this.nAttr=header.length
    this.nInputs=this.nAttr-1
    this.attributes=new ArrayBuffer[MyAttribute](this.nAttr)
    var possibleValues:ArrayBuffer[ArrayBuffer[String]]=this.readPossiblesValues(file)

    for(i<- 0 until this.nInputs){
      var a : MyAttribute =null
      if(!isDouble(possibleValues(i).apply(0))){
        a= new MyAttribute(header(i),possibleValues(i),i)
        a.setRange(0.0D,(a.numValues-1).toFloat)
      }else{
        a= new MyAttribute(header(i), if(isInteger(possibleValues(i).apply(0)))1 else 2,i)
        a.setRange(java.lang.Double.parseDouble(possibleValues(i).apply(0)),java.lang.Double.parseDouble(possibleValues(i).apply(1)))
      }
      this.attributes.addOne(a)
    }
    var a :MyAttribute= new MyAttribute(header(this.nInputs),possibleValues(this.nInputs),this.nInputs)
    a.setRange(0.0D,a.numValues-1)
    this.attributes.addOne(a)
    this.nClasses=this.attributes(this.nInputs).numValues

  }


  private def readInstances(file:ArrayBuffer[Array[String]]): Unit = {
    var numInstances : Int = file.size
    this.instances= new ArrayBuffer[MyInstance](numInstances)
    this.classSupp= new Array[Int](this.nClasses)
    file.foreach { line =>
      val values = new Array[Double](nAttr)
      for (i <- 0 until nInputs) {
        val v = line(i)
        values(i) = if (v.isEmpty) Double.MaxValue else {
          val attr = attributes(i)
          if (attr.isNominal) attr.indexOf(v).toDouble else v.toDouble
        }
      }
      val v = line(nInputs)
      val classValue = if (v.isEmpty) Int.MaxValue else attributes(nInputs).indexOf(v)
      addInstance(new MyInstance(values, classValue))
    }

  }

  private def readPossiblesValues(file:ArrayBuffer[Array[String]]):ArrayBuffer[ArrayBuffer[String]]={
    var possibleValues:ArrayBuffer[ArrayBuffer[String]]= new ArrayBuffer[ArrayBuffer[String]](this.nAttr)
    for (i<-0 until this.nAttr)
     possibleValues.addOne(new ArrayBuffer())
    for (line <- file){
      for (i<- 0 until this.nAttr){
        var value =line(i)
        if(value.nonEmpty){
          var pv :ArrayBuffer[String]=possibleValues(i)
          try{
            var v1 :Double= java.lang.Double.parseDouble(value)
            if(pv.isEmpty)
              pv.addOne(value)
            else{
              var v2:Double=java.lang.Double.parseDouble(pv(0))
              
              if(pv.size==1){
                if(v1 !=v2)
                  pv.insert(if(v1>v2) pv.size else 0,value)
              }else{
                if(v2>v1) pv.insert(0,value)
                else if (java.lang.Double.parseDouble(pv(1))<v1)
                  pv.insert(1,value)
                  
              }
            }
          }catch
            case _:NumberFormatException=> if(!pv.contains(value)) pv.addOne(value)
        }
      }
    }
    possibleValues
  }
  

  private def copyInstance(dest: MyDataset)= {
    dest.instances.foreach(addInstance)
  }

  def isInteger(s: String): Boolean = try {
    s.toInt
    true
  } catch {
    case _: NumberFormatException => false
  }

  def isDouble(s: String): Boolean = try {
    s.toDouble
    true
  } catch {
    case _: NumberFormatException => false
  }

  def addInstance(inst : MyInstance)={
    instances :+ inst
    classSupp(inst.clasValue())+=1
  }

  def getAttribute(index:Int):MyAttribute={
    attributes.apply(index)
  }

  def  getAttributes: ArrayBuffer[MyAttribute]= this.attributes
  def getClassIndex : Int =nInputs

  def getClassesSupp(pos: Int): Int = classSupp(pos)
  def getClasesSupp : Array[Int]=classSupp
  def getInstance(pos: Int ):MyInstance=instances(pos)
  def getInstances:ArrayBuffer[MyInstance]=instances
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
