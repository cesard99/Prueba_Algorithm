package Repository

class MyInstance(val value: Array[Double], val classValue: Int) {

  // Devuelve el valor de la clase
  def clasValue(): Int = classValue

  // Devuelve los valores del atributo
  private def values(): Array[Double] = value

  // Devuelve el valor en el índice especificado
  def values(index: Int): Double = value(index)

  // Verifica si el valor en un índice es un valor perdido
  def isMissing(index: Int): Boolean = value(index).isNaN

  // Devuelve el número de valores
  def numValues(): Int = value.length

  // Marca un valor como perdido
  def setMissing(index: Int): Unit = setValue(index, MyInstance.MISSING_VALUE)

  // Establece un valor en un índice específico
  private def setValue(index: Int, values: Double): Unit = {
    value(index) = values
  }

  // Devuelve una representación en cadena del objeto
  override def toString: String = {
    value.mkString("[", ", ", "]")
  }
}
// Objeto compañero para constantes
private object MyInstance {
  private val MISSING_VALUE: Double = Double.NaN
}
