package Forest

import Repository.MyDataset
import scala.util.Random

class ChooseAttribute( private val dataset: MyDataset,private val instanceIndexes: Array[Int], private val attributeUsed: Array[Boolean]) {
  val c: Int = 0
  var interval: Interval = _

  def run(): Unit = {
    var i = Random.nextInt( dataset.getNumInputs) // Randomiza el valor de i
    val j = i // Se guarda el valor de i para comparar en la condición de salida

    // Aseguramos que el bucle se ejecute al menos una vez dando el sentido como si fuera un 
    var firstIteration = true

    while (firstIteration || i != j) {
      firstIteration = false

      if (!attributeUsed(i)) {
        val chooseInterval = new ChooseInterval(dataset, instanceIndexes, dataset.getAttribute(i))
        chooseInterval.run()

        if (chooseInterval.getInterval.getInfoGain != -1) {
          interval = chooseInterval.getInterval // Actualiza el intervalo si encontramos uno válido
          return // Terminamos la ejecución si encontramos un intervalo válido
        }
      }

      i = if (i == dataset.getNumInputs - 1) 0 else i + 1
    }
  }

  def getInterval:Interval= interval

}