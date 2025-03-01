package Population
import Utils.{Configuration,WeightsArray}
import Repository.MyDataset



abstract class PopulationConstructor(protected val conf: Configuration,
                            protected val weights: WeightsArray,
                            protected val dataset: MyDataset,
                            protected val update: IUpdatePopulation,
                            protected val UP: UpdatedPopulation) {
  
  protected var trials:Int=0
  def getTrials:Int=trials
  def run(): Unit
  def printIndividuals():Unit
}
