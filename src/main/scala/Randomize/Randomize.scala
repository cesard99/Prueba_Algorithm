package Randomize

import scala.annotation.tailrec

object Randomize {
  private val generator:MTwister=null

  def setSeed(seed: Long): Unit = {
   var generator: MTwister = new MTwister
   generator.initGenerateRandom(seed)
  }

  private def rand(): Double = this.synchronized {
    generator.generateRandomRes53
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
