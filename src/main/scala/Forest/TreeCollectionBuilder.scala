package Forest
import Codification.{Individual,Chromosome}
import Utils.DoubleCompare
import Utils.Randomize
import Repository.MyDataset
import Repository.MyAttribute
import Utils.Configuration

import java.util
import java.util.LinkedList
import java.util.stream.IntStream
import scala.util.Random

class TreeCollectionBuilder(private val conf:Configuration, private val dataset: MyDataset ) {
  private val THREAD : Int = Runtime.getRuntime.availableProcessors()
  private var individuals: util.LinkedList[Individual]= new util.LinkedList()

  def build(numIndividuals: Int ):Unit ={
    individuals.clear()
    val size : Int = dataset.getNumInstances
    val minInstances :Int = (conf.getMinInstances * size).toInt

    IntStream.range(0,THREAD).parallel().forEach( t =>{
      while (individuals.size()< numIndividuals){
        var trainBag: Array[Int] = Array(size)

        for (i<- 0 until size ){
          trainBag(i)=Random.nextInt(size)
        }
        val constructor: TreeBuilder = new TreeBuilder(dataset, trainBag, minInstances)
        covert(constructor.build, new Chromosome(dataset))
      }
      })
  }

  def covert (tree: Node , chromosome: Chromosome):Unit={
   if(!tree.isLeaf){
     val sons : Array[Node]=tree.getChildredn
     val interval :Interval = tree.getInterval

     for(i<- sons.indices){
       buildInvolvedGene(interval,i,chromosome)

       if(sons(i).isLeaf){
         val individual : Individual= buildIndividual(chromosome)

         if(individual !=null){
           individuals.synchronized{
             individuals.add(individual)
           }
         }
       }else
         covert(sons(i),chromosome)
     }

      chromosome.setInvolved(false,interval.getAttributeIndex)
   }

  }

  private def buildInvolvedGene(interval: Interval, index: Int, chromosome: Chromosome):Unit={
    val attrIndex =interval.getAttributeIndex
    chromosome.setLowerBound(interval.getLowerBound,attrIndex)
    chromosome.setUpperBound(interval.getupperBound,attrIndex)
    chromosome.setPositiveInterval(index==0,attrIndex)
    chromosome.setInvolved(true,attrIndex)
  }

  private def buildIndividual(chrom: Chromosome):Individual={

    val chromosome: Chromosome = chrom.clone()
    val individual: Individual= new Individual(conf,dataset,chromosome)
    individual.computeObjetives()

    if(individual.getMetrics.getWracc>0){
      val numGenes: Int= dataset.getNumInputs
      for(i<- 0 until numGenes){
        if(!chromosome.isInvolved(i))
          buildUninvolvedGene(individual,i)
      }
      return individual
    }
    null
  }

  private def   buildUninvolvedGene(individual: Individual, index: Int):Unit={
    val chromosome: Chromosome= individual.getChromosome
    chromosome.setPositiveInterval(Random.nextBoolean(),index)

    val covered : Array[Int]= individual.getMetrics.getCovered
    val instanceIndex = covered(Random.nextInt(covered.length))
    val value : Double= dataset.getInstance(instanceIndex).values(index)
    
    var lb : Double=0.0
    var ub : Double=0.0
    
    val a :MyAttribute=dataset.getAttribute(index)
    
    if(a.isNominal) {
      lb = value
      ub = lb
    }else{
      val amplitude: Double = a.getAmplitude / (conf.getAmplitudeFactor * 4)
      
      if(chromosome.isPositiveInterval(index)){
        lb= Math.max(value-amplitude,a.getLowerBound)
        ub=Math.min(value+ amplitude,a.getUpperBound)
      }else{
        var movement : Double=0.0
        if(DoubleCompare().greater(value-a.getLowerBound,a.getUpperBound-value)){
          movement=value-a.getLowerBound-amplitude
          lb=value-movement
          ub=a.getLowerBound+movement
          
        }else{
          movement=a.getUpperBound-value-amplitude
          lb=a.getUpperBound-movement
          ub=value+movement
        }
        
      }
      if(a.isInteger){
        lb=lb.toInt
        ub=ub.toInt
      }
    }
    chromosome.setLowerBound(lb,index)
    chromosome.setUpperBound(ub,index)
  }
  
  def getIndividuals:util.LinkedList[Individual]=individuals

}
