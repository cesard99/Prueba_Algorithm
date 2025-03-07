package Randomize

class MTwister {
  private val state: Array[Long] = new Array[Long](624)
  var left: Int = 1
  private var initf: Int = 0
  private var inext: Int = 0

  def initGenerateRandom(s: Long): Unit = {
    state(0) = s & 0xffffffffL
    for (j <- 1 until 624) {
      state(j) = 1812433253L * (state(j - 1) ^ (state(j - 1) >> 30)) + j
      state(j)  &= 0xffffffffL
    }
    left = 1
    initf = 1
  }

  private def nextState(): Unit = {
    var ip = 0
    var j = 0

    if (initf == 0) initGenerateRandom(5489L)

    left = 624
    inext = 0

    for (j <- 1 until (624 - 397 + 1)) {
      state(ip) = state(ip + 397) ^ ((((state(ip) & 0x80000000L) | (state(ip + 1) & 0x7fffffffL)) >> 1) ^ (if ((state(ip + 1) & 1L) != 0L) 0x9908b0dfL else 0L))
      ip += 1
    }

    for (j <- 1 until 397) {
      state(ip) = state(ip + 397 - 624) ^ ((((state(ip) & 0x80000000L) | (state(ip + 1) & 0x7fffffffL)) >> 1) ^ (if ((state(ip + 1) & 1L) != 0L) 0x9908b0dfL else 0L))
      ip += 1
    }

    state(ip) = state(ip + 397 - 624) ^ ((((state(ip) & 0x80000000L) | (state(0) & 0x7fffffffL)) >> 1) ^ (if ((state(0) & 1L) != 0L) 0x9908b0dfL else 0L))
  }

  private def generateRandomInt32: Long = {
    if ( {
      left -= 1; left
    } == 0) nextState()
    val temporal = inext + 1
    var y = state(temporal)
    inext += 1
    y ^= (y >> 11)
    y ^= (y << 7) & 0x9d2c5680L
    y ^= (y << 15) & 0xefc60000L
    y ^= (y >> 18)
    y
  }

  // Genera un número aleatorio con 53 bits de resolucion
  def generateRandomRes53:Double={
    val a: Long = generateRandomInt32 >> 5
    val b: Long = generateRandomInt32 >> 6
    (a * 67108864.0 + b) * (1.0 / 9007199254740992.0)
  }
}