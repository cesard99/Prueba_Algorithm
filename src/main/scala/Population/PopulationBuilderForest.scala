package Population
import Codification.{Individual, Metrics}
import Utils.{Configuration, WeightsArray}
import Repository.MyDataset
import Forest.TreeCollectionBuilder

import java.util

class PopulationBuilderForest(conf: Configuration,
                              weights: WeightsArray,
                              dataset: MyDataset,
                              validate: IUpdatePopulation,
                              lastPopulation: UpdatedPopulation) extends PopulationConstructor(conf, weights, dataset, validate, lastPopulation) {

  
  private val builder = new TreeCollectionBuilder(conf, dataset)
  var individuals: util.LinkedList[Individual]=_

  override def run(): Unit = {
    UP.clear()
    val sizePop = weights.size()

    while (UP.size != sizePop) {
      if (individuals == null || individuals.isEmpty) {
        builder.build(weights.size() - UP.size)
        individuals = builder.getIndividuals
      }

      while (!individuals.isEmpty && UP.size != sizePop) {
        val ind = individuals.remove()

        val m = ind.getMetrics
        m.setWeights(weights.getWeight(UP.size))
        m.setNeighbors(weights.getWeightNeighbors(UP.size))

        trials + 1

        if (UP.noneEqualIndividual(ind)) {
          update.execute(UP, ind)
        }
      }
    }
  }

}
