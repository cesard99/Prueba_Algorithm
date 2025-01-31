package Population

import Codification.Individual

trait IUpdatePopulation {
  def execute(lastPopulation: UpdatedPopulation, individual: Individual): Unit
}
