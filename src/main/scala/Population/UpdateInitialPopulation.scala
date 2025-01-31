package Population
import Codification.Individual

class UpdateInitialPopulation extends IUpdatePopulation {

  override def execute(up: UpdatedPopulation, ind: Individual): Unit = {
    up.addIndividual(ind)
  }
}
