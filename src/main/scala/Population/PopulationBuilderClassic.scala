package Population
import Codification.{Chromosome, Individual}
import Randomize.Randomize
import Repository.{MyAttribute, MyDataset, MyInstance}
import Utils.{Configuration, DoubleCompare, Util, WeightsArray}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class PopulationBuilderClassic(conf: Configuration,
                               weights: WeightsArray,
                               dataset: MyDataset,
                               update: IUpdatePopulation,
                               UP: UpdatedPopulation) extends PopulationConstructor(conf, weights, dataset, update, UP){

  
  override def run(): Unit = {
    UP.clear()

    var notCovered =ArrayBuffer.empty[Int]
    
    while (UP.size != weights.size()) {
      if (notCovered.isEmpty) {
        for (i <- 0 until dataset.getNumInstances) {
          notCovered+=i
        }
      }

      val ind: Individual = buildIndividual(notCovered(scala.util.Random.nextInt(notCovered.size)))
          ind.computeObjetives()


      ind.getMetrics.setWeights(weights.getWeight(UP.size))
      ind.getMetrics.setNeighbors(weights.getWeightNeighbors(UP.sizze))

        trials+1

      if(DoubleCompare().greater(ind.getMetrics.getWracc,0) && UP.noneEqualIndividual(ind)){
        update.execute(UP,ind)
        deleteTransCovered(ind, notCovered)
      }

    }
  }

  private def buildIndividual(index: Int):Individual={
    var lb : Double=0.0
    var ub:Double=0.0
    var value : Double=0.0
    var instance : MyInstance= dataset.getInstance(index)
    var chromosome :Chromosome = new Chromosome(dataset)

    val rndAttr : Array[Int]= Util.randomPermutation(dataset.getNumInputs)
    val nUsedGenes = Randomize.randIntClosed(1,dataset.getNumInputs)

    for (i <- 0 until nUsedGenes) {
      val a = dataset.getAttribute(rndAttr(i))

      chromosome.setInvolved(true, a.getIndex)
      chromosome.setPositiveInterval(Random.nextBoolean(), a.getIndex)

      value = instance.value(a.getIndex)

      if (!a.isNominal) {
        val amplitude = a.getAmplitude / (conf.getAmplitudeFactor * 4)
        val min = a.getLowerBound
        val max = a.getUpperBound

        if (chromosome.isPositiveInterval(a.getIndex)) {
          lb = Math.max(value - amplitude, min)
          ub = Math.min(value + amplitude, max)
        } else {
          if (value - min > max - value) {
            lb = min + amplitude
            ub = min + value - lb
          } else {
            lb = value + amplitude
            ub = value + max - lb
          }
        }

        if (a.isInteger) {
          lb = lb.toInt
          ub = ub.toInt
        }
      } else {
        if (value < 0) {
          value = Randomize.randIntClosed(a.getLowerBound.toInt, a.getUpperBound.toInt)
          chromosome.setPositiveInterval(false, a.getIndex)
        }
        ub = value.toInt
        lb = ub
      }

      chromosome.setLowerBound(lb, a.getIndex)
      chromosome.setUpperBound(ub, a.getIndex)
    }

    for (i <- nUsedGenes until dataset.getNumInputs) {
      val a = dataset.getAttribute(rndAttr(i))

      chromosome.setInvolved(false, rndAttr(i))
      chromosome.setPositiveInterval(Random.nextBoolean(), rndAttr(i))

      value = Randomize.randDoubleClosed(a.getLowerBound, a.getUpperBound)

      if (!a.isNominal) {
        val amplitude = a.getAmplitude / (conf.getAmplitudeFactor * 4)

        lb = Math.max(value - amplitude, a.getLowerBound)
        ub = Math.min(value + amplitude, a.getUpperBound)

        if (a.isInteger) {
          lb = lb.toInt
          ub = ub.toInt
        }
      } else {
        ub = Random.nextInt(a.numValues)
        lb = ub
      }

      chromosome.setLowerBound(lb, rndAttr(i))
      chromosome.setUpperBound(ub, rndAttr(i))
    }

    new Individual(conf, dataset, chromosome)
  }

  private def deleteTransCovered(ind: Individual, notCovered: ArrayBuffer[Int]): Unit = {
    for (i <- (notCovered.size - 1) to 0 by -1) {
      val example: MyInstance = dataset.getInstance(notCovered(i))

      if (ind.getChromosome.isCovered(example)) {
        notCovered.remove(i)
      }
    }
  }



}
