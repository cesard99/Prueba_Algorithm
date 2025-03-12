package Forest
import Codification.{Chromosome, Individual}
import Repository.{MyAttribute, MyDataset}
import Utils.{Configuration, DoubleCompare}

import java.util.*
import java.util.stream.IntStream
import scala.collection.mutable
import scala.math
import scala.util.Random




class TreeCollectionBuilder(private val conf:Configuration, private val dataset: MyDataset ) {
  private val THREAD : Int = Runtime.getRuntime.availableProcessors()
  private val individuals: mutable.Queue[Individual] = mutable.Queue.empty[Individual]

  def build(numIndividuals: Int ):Unit ={ // funcion que permite crear un conjunto de individuos usando un enfoque de Muestreo con reemplazo.
    individuals.clear()
    val size : Int = dataset.getNumInstances //se obtiene el tamaño del numero de instancias(filas que hay en el dataset)
    val minInstances :Int = (conf.getMinInstances * size).toInt

    IntStream.range(0,THREAD).parallel().forEach( t =>{
      while (individuals.size < numIndividuals){
        val trainBag: Array[Int] = new Array[Int](size)

        for (i<- 0 until size )
          trainBag(i)=Random.nextInt(size)

        val constructor: TreeBuilder = new TreeBuilder(dataset, trainBag, minInstances)
        covert(constructor.build, new Chromosome(dataset))
      }
    })
  }

  private def covert(tree: Node, chromosome: Chromosome):Unit={
    if(!tree.isLeaf){ // el nodo no es hoja
      val sons : Array[Node]=tree.getChildredn
      val interval :Interval = tree.getInterval

      for(i<- sons.indices){
        buildUsedGene(interval,i,chromosome)

        if(sons(i).isLeaf){ // si el hijo es hoja mandamos a crear el individuo
          val individual : Individual= buildIndividual(chromosome)

          individuals.synchronized{
            individuals.addOne(individual)
          }
          chromosome.setUsedGene(false,interval.getAttribute.getIndex)
        }else{
          chromosome.setUsedGene(false,interval.getAttribute.getIndex)
          covert(sons(i),chromosome)
        }
      }
    }
  }



  private def buildUsedGene(interval: Interval, index: Int, chromosome: Chromosome):Unit={
    val attrIndex =interval.getAttribute.getIndex
    chromosome.setLowerBound(interval.getLowerBound,attrIndex)
    chromosome.setUpperBound(interval.getupperBound,attrIndex)
    chromosome.setPositiveInterval(index==0,attrIndex)
    chromosome.setUsedGene(true,attrIndex)
  }

  private def buildIndividual(chromosome: Chromosome):Individual={
    val clonedChromosome = chromosome.clone()
    val individual : Individual = new Individual(conf,dataset,clonedChromosome)
    individual.computeObjetives()

    val numGenes = dataset.getNumInputs
    for(i <- 0 until numGenes)
      if(!chromosome.isUsedGene(i))
        buildUnusedGene(individual,i)
    
    individual
  }

  private def  buildUnusedGene(individual: Individual, index: Int):Unit={
    val a : MyAttribute = dataset.getAttribute(index)

    val chromosome: Chromosome = individual.getChromosome
    chromosome.setUsedGene(false,index)
    chromosome.setPositiveInterval(Random.nextBoolean(),index)

    val covered :Array[Int]= individual.getMetrics.getCovered
    val instaceIndex : Int = covered(Random.nextInt(covered.length))
    val value : Double= dataset.getInstance(instaceIndex).values(index)

    var lb : Double=0
    var ub : Double=0

    if(a.isNominal){
      lb=value
      ub= lb
    }else{
      val amplitude:Double= a.getAmplitude / (conf.getAmplitudeFactor*4)

      if(chromosome.isPositiveInterval(index)){
        lb=math.max(value-amplitude,a.getLowerBound)
        ub=math.min(value+amplitude,a.getUpperBound)
      }else{
        var movement:Double=0
        if(DoubleCompare().greater(value-a.getLowerBound,a.getUpperBound-value)){
          movement= value-a.getLowerBound-amplitude
          lb=value-movement
          ub= a.getLowerBound+movement
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

  def getIndividuals: mutable.Queue[Individual]=individuals

}
