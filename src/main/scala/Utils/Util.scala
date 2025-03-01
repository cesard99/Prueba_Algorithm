package Utils
import scala.util.Random

object Util {
  def intersection(array1: Array[Int], array2: Array[Int]): Int = {
    var c = 0
    var i = 0
    var j = 0


    val size1 = array1.length
    val size2 = array2.length
    var v1 :Int=0
    var v2 :Int=0

    while (i < size1 && j < size2) {
      v1=array1(i)
      v2=array2(i)

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
    val index = new Array[Int](size)
    for(i <- 0 until size)
      index(i)=i
    var temp: Int= 0
    var j:Int=0

    for(i<-0 until size){
      j=Random.nextInt(size)

      temp =index(i)
      index(i)= index(j)
      index(j)=temp
    }
    index
  }

  def log2(num: Double): Double = {
    if (num > -1e-6 && num < 1e-6) 0 else num * (math.log(num) / math.log(2))
  }
  
  def swap(array: Array[Int], i: Int,j: Int):Unit={
    val aux: Int = array(j)
    array(j)=array(i)
    array(i)=aux
  }
  
}







