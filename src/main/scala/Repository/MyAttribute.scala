package Repository

import scala.collection.mutable.ArrayBuffer

class MyAttribute(val name: String, val attributeType: Int, private var _index: Int) {



  private var values: ArrayBuffer[String] = ArrayBuffer.empty[String]
  private var lowerBound: Double = 0.0
  private var upperBound: Double = 0.0
  private var amplitude: Double = 0.0

  // Constructor secundario para atributos nominales
  def this(name: String, values: ArrayBuffer[String], index: Int) = {
    this(name, MyAttribute.NOMINAL, index)
    this.values = values
  }


  // Métodos de acceso a valores
  def indexOf(value: String): Int = values.indexOf(value)
  def numValues: Int = values.size
  def value(index: Int): String = values(index)

  // Métodos para manejar el rango
  def setRange(lowerBound: Double, upperBound: Double): Unit = {
    this.lowerBound = lowerBound
    this.upperBound = upperBound
    this.amplitude = upperBound - lowerBound
  }

  def getLowerBound: Double = lowerBound
  def getUpperBound: Double = upperBound
  def getAmplitude: Double = amplitude

  def getType: Int = attributeType
  def getIndex: Int = _index
  def setIndex(index: Int): Unit = {
    _index = index
  }

  // Métodos estáticos
  def isNominal: Boolean = attributeType == MyAttribute.NOMINAL

  def isReal: Boolean = attributeType == MyAttribute.REAL

  def isInteger: Boolean = attributeType == MyAttribute.INTEGER
}

object MyAttribute {
  // Constantes de tipo
  val NOMINAL: Int = 0
  val INTEGER: Int = 1
  val REAL: Int = 2
  
}
