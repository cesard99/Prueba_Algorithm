package Population
import Utils.{Configuration,WeightsArray}
import Repository.MyDataset



class PopulationConstructor(protected val conf: Configuration,
                            protected val weights: WeightsArray,
                            protected val dataset: MyDataset,
                            protected val update: IUpdatePopulation,
                            protected val UP: UpdatedPopulation) {
  
  protected val trials:Int
  def getTrials:Int=trials
  def run(): Unit
  
}
