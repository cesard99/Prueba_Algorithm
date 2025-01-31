package dataset

/**
 * Class for representing an attribute of the received dataset 
 * 
 * @param _name Name of the attribute
 * @param _type Type (Integer, Real or Nominal) of the attribute
 * @param _values Possible values for the attribute (empty if not nominal)
 * @param _lowerBound Lower bound of the attribute values (0 if Nominal)
 * @param _upperBound Upper bound of the attribute values (amount of values if Nominal)
 * @param _index Index of the attribute in the Dataset class
 */
class Attribute(
                 private val _name: String,
                 private val _type: AttributeType,
                 private var _values: Array[String],
                 private var _lowerBound: Double,
                 private var _upperBound: Double,
                 private var _index: Int
               ) {
  private var _amplitude: Double = _upperBound - _lowerBound
  
  def name: String = _name

  def attributeType: AttributeType = _type

  def values: Array[String] = _values

  def values_=(values: Array[String]): Unit = _values = values

  def lowerBound: Double = _lowerBound

  def lowerBound_=(lowerBound: Double): Unit = _lowerBound = lowerBound

  def upperBound: Double = _upperBound

  def upperBound_=(upperBound: Double): Unit = _upperBound = upperBound

  def amplitude: Double = _amplitude

  def amplitude_=(amplitude: Double): Unit = _amplitude = amplitude

  def index: Int = _index

  def index_=(index: Int): Unit = _index = index

  def numValues: Int = _values.length

  def value(index: Int): String = _values(index)

  def setRange(lowerBound: Double, upperBound: Double): Unit = {
    _lowerBound = lowerBound
    _upperBound = upperBound
    _amplitude = upperBound - lowerBound
  }

  def indexOf(value: String): Int = _values.indexOf(value)

}

/**
 * Object for interfacing the creation of an attribute instance with flexible parameters
 */
object Attribute {
  def apply(
             _name: String, 
             _type: AttributeType, 
             _values: Array[String] = Array.empty[String], 
             _lowerBound: Double = 0, 
             _upperBound: Double, 
             _index: Int = 0
           ): Attribute = new Attribute(_name, _type, _values, _lowerBound, _upperBound, _index)
}

sealed trait AttributeType

case object INTEGER extends AttributeType

case object REAL extends AttributeType

case object NOMINAL extends AttributeType
