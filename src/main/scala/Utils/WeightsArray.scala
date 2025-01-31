package Utils





class WeightsArray {
  private  var T: Int =0
  private var H : Int =0
  private  var weights : Array[Array[Double]]=_
  private var weightsNeighbors :Array[Array[Int]]= _

  def weightsArray(t:Int,h:Int): Unit = {
    this.T=t
    this.H=h

    val size = (h + 1) * (h + 2) / 2

    this.weights=Array.ofDim[Double](size,3)
    this.weightsNeighbors=Array.ofDim[Int](size,T)
  }

  def compute():Unit={
    computeWeights()
    computeWeightsNeighbors()
  }

  private def computeWeights():Unit={
    val v: Double = H.toDouble
    var c: Int =0
    for (i <- 0 to H) {
      for (j <- 0 until H - i) {
        weights(c)(0) = i.toDouble / v
        weights(c)(1) = j.toDouble / v
        weights(c)(2) = (v - i - j) / v
        c += 1
      }
    }
  }
  private def computeWeightsNeighbors():Unit={
    val distance = computeEuclidean()
    for (i <- weights.indices) {
      weightsNeighbors(i) = closestVector(distance(i), i)
    }
  }

  private def closestVector(dist: Array[Double], vector: Int): Array[Int]={
    val size = dist.length
    val index = Array.range(0, size)

    for (i <- 1 until size) {
      for (j <- 0 until  size-i) {
        if (DoubleCompare().less(dist(index(j + 1)), dist(index(j)))){
          Util.swap(index, j, j + 1)
        }

        }
      }
    val closest = Array.ofDim[Int](T)
    var i = 0
    for (j <- 0 until T if index(j) != vector) {
      closest(i) = index(j)
      i += 1
    }
    closest
  }

  private def computeEuclidean(): Array[Array[Double]] = {
    val size = weights.length
    val distance = Array.ofDim[Double](size, size)

    for (i <- 0 until size; j <- i + 1 until size) {
      distance(j)(i) = computeDistance(weights(i), weights(j))
      distance(i)(j) = distance(j)(i)
    }

    distance
  }

  private def computeDistance(weightsA: Array[Double], weightsB: Array[Double]): Double = {
    weightsA.zip(weightsB).map { case (a, b) =>
      Math.pow(a - b, 2.0)
    }.sum
  }

  def getWeight(index: Int): Array[Double] = weights(index)

  def getWeightNeighbors(index: Int): Array[Int] = weightsNeighbors(index)

  def size(): Int = weights.length



}