package Randomize

object Randomize {
  private var generator:MTwister=_

  def setSeed(seed: Long): Unit = {
    generator = new MTwister
    generator.initGenerateRandom(seed)
  }

  private def rand(): Double = this.synchronized {
     return generator.generateRandomRes53
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
}
