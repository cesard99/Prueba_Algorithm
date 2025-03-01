package Utils
import Population.{PopulationBuilderForest, PopulationConstructor, UpdateInitialPopulation, UpdatedPopulation}
import Repository.MyDataset

class Init(conf:Configuration) {
  val configuration:Configuration=conf
  val dataset = new MyDataset(conf.getInputFileTra,conf.getCsvDelimiter)
  val weights = new WeightsArray(conf.getT,conf.getH)
  weights.compute()
  
  var z: Array[Double]=new Array[Double](conf.getNumObjetives)
  var zMin :Array[Double]= new Array[Double](conf.getNumObjetives)
  var UP :UpdatedPopulation=new UpdatedPopulation(weights.size(),z,zMin)
  private val initialBuilder: PopulationConstructor = new PopulationBuilderForest(conf, weights, dataset, new UpdateInitialPopulation, UP)

  def run (): Unit ={
    initialBuilder.run()
    initialBuilder.printIndividuals()
  }

}
