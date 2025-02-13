package Utils

import scala.annotation.tailrec

object Randomize {



  @tailrec
   def setSeed(seed: Long): Unit = {
   Utils.Randomize.setSeed(seed)
  }

  private def rand(): Double = this.synchronized {
    Utils.Randomize.rand()
  }

  def randIntOpen(lb: Int, ub: Int): Int = {
    ((lb + 1) + (ub - (lb + 1)) * rand()).toInt
  }

  def randIntClosed(lb: Int, ub: Int): Int = {
    (lb + (ub + 1 - lb) * rand()).toInt
  }

  def randDouble(lb: Double, ub: Double): Double = {
    lb + (ub - lb) * rand()
  }

  def randDoubleOpen(lb: Double, ub: Double): Double = {
    lb + (ub - lb) * rand()
  }

  def randDoubleClosed(lb: Double, ub: Double): Double = {
    lb + (ub - lb) * rand()
  }

  private def initializeSeed(seed: Long): Unit = {
    Randomize.setSeed(seed) // Ahora no hay conflicto de nombres
  }
}
