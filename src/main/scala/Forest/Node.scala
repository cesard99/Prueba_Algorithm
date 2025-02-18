package Forest

//Aqui se inicializa la clase y como es un nodo hoja se le pasa el intervalo
class Node( private val interval: Interval) {
  private val children: Array[Node] = new Array[Node](2)
  private var leaf : Boolean= false
  
  // constructor para un nodo hoja 
  def this() = { 
    this(null)
    this.leaf=true
  }
  
  def getChildredn:Array[Node]=children
  def addChild(index: Int , child: Node):Unit= children(index)=child
  def isLeaf:Boolean= leaf
  def getInterval: Interval = interval

}
