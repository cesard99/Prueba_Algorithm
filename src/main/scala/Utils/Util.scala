package Utils

object Util {
  def intersection(array1: Array[Int], array2: Array[Int]): Int = {
    var c = 0
    var i = 0
    var j = 0


    val size1 = array1.length
    val size2 = array2.length

    while (i < size1 && j < size2) {
      val v1 = array1(i)
      val v2 = array2(j)

      if (v1 == v2) {
        c += 1
        i += 1
        j += 1

      } else if (v1 < v2) {
        i += 1
      } else {
        j += 1
      }
    }
    c

  }
  def randomPermutation(size : Int):Array[Int]={
    val index = Array.tabulate(size)(i => i)
    for (i <- 0 until size) {
      val j = scala.util.Random.nextInt(size) //Genra un numero aleatorio entre el 0 y el size
      val temp = index(i)
      index(i) = index(j)
      index(j) = temp
    }
    index
  }

  def log2(num: Double): Double = {
    if (num > -1e-6 && num < 1e-6) 0.0
    else num * (math.log(num) / math.log(2))
  }
  
  def swap(array: Array[Int], i: Int,j: Int):Unit={
    val aux: Int = array(j)
    array(j)=array(i)
    array(i)=aux
  }
  
}







