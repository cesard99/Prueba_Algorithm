package Population
import Codification.Individual
import Forest.TreeCollectionBuilder
import Repository.MyDataset
import Utils.{Configuration, WeightsArray}

import scala.collection.mutable
//Clase que se encarga de crear una poblacion de arboles 
class PopulationBuilderForest(conf: Configuration,
                              weights: WeightsArray,
                              dataset: MyDataset,
                              validate: IUpdatePopulation,
                              lastPopulation: UpdatedPopulation) extends PopulationConstructor(conf, weights, dataset, validate, lastPopulation) {

  
  private val builder = new TreeCollectionBuilder(conf, dataset)
  var individuals: mutable.Queue[Individual]=_

  
  override def run(): Unit = {
    UP.clear() //Limpia la poblacion y la prepara para crear una nueva poblacion 
    val sizePop = weights.size() //Se obtiene el tamaño de la poblacion actual que se espera 

    while (UP.sizze != sizePop) { // Este bucle es para crear individuos, mientras el tamaño de la poblacion sea diferente del tamaño esperado 
      if (individuals == null || individuals.isEmpty) {//se verifica si la lista de individuos esta vacia o null
        builder.build(weights.size() - UP.sizze)// si se cumple lo anterior se crean nuevos individuos
        individuals = builder.getIndividuals
      }

      while (individuals.nonEmpty && UP.sizze != sizePop) { // Este bucle se cumple mientras haya individuos y el tamaño de la poblacion no sea el esperado
        val ind = individuals.dequeue() // se desencola el primer individuo y se almacena
        // se configuran las metricas de ese individuo
        val m = ind.getMetrics
        m.setWeights(weights.getWeight(UP.sizze))
        m.setNeighbors(weights.getWeightNeighbors(UP.sizze))

        trials += 1 // se incrementa el contador en 1
        
        
        // se Verifica que que no exista un individuo igual en la poblacion y en caso de que no exista se actualiza
        if (UP.noneEqualIndividual(ind)) {
          update.execute(UP, ind)
        }
      }
    }
  }
  def printIndividuals():Unit={
    for(i<- individuals.indices)
      println(individuals(i))
  }



}
