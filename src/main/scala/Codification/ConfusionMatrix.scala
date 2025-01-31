package Codification

class ConfusionMatrix(private var tp :Int , private var tn : Int , private var fp : Int , private var fn : Int) {

  def getTp: Int = tp

  def setTp(tp: Int): Unit = this.tp = tp

  def getTn: Int = tn

  def setTn(tn: Int): Unit = this.tn = tn

  def getFp: Int = fp

  def setFp(fp: Int): Unit = this.fp = fp

  def getFn: Int = fn

  def setFn(fn: Int): Unit = this.fn = fn

  def antecedentCovered(): Int = tp + fp

  def consequentCovered(): Int = tp + fn

  def total(): Int = tp + fp + tn + fn

  def copy(): ConfusionMatrix = new ConfusionMatrix(tp, tn, fp, fn)
  
}
