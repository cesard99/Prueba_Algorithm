package Forest

import Repository.MyDataset
import scala.util.Random

class ChooseAttribute( private val dataset: MyDataset,private val instanceIndexes: Array[Int], private val attributeUsed: Array[Boolean]) {
  val c: Int = 0
  var interval: Interval = _

  def run(): Unit = {
    var i = Random.nextInt(dataset.getNumInputs)
    val j = i

    //Se usa un While-do Porque ya a partir de scala 3.0 se descontinuo el do-While
    while ( {
      if (!attributeUsed(i)) {
        val chooseInterval = new ChooseInterval(dataset, instanceIndexes, i)
        chooseInterval.run()

        if (chooseInterval.getInterval.getSubsets != null) {
          interval = chooseInterval.getInterval
        }
      }

      i = if (i == dataset.getNumInputs - 1) 0 else i + 1

      i != j && interval == null
    }) {
      ()
    }


  }

  def getInterval:Interval= interval

}