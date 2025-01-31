package dataset

/**
 * Class for representing an instance in the received dataset
 * 
 * @param values List of values for each attribute
 * @param classValue Index if the instance class value in the class attribute 
 */
case class Instance(
                     values: Array[Option[Double]], 
                     classValue: Double,
                     index: Int
                   ) {

  def valueAt(index: Int): Option[Double] = values(index)

  def numValues(): Int = values.length

  override def toString: String = {
    val result = new StringBuilder("[")
    values.init.foreach(value => result.append(value).append(", "))
    result.append(values.last)
    result.append("]")
    result.toString
  }
}
