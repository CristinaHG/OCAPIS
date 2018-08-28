package cristinahg.ocapis

class NeighborWeight( var weight:Double = .0, var index:Int = -1) extends Comparable[_] {

  def this(_ind: Int, _weight: Double) {
    this()
    weight = _weight
    index = _ind
  }

  def getWeight: Double = weight

  def setWeight(_weight: Double): Unit = {
    weight = _weight
  }

  override def compareTo(o: Any): Int = {
    val n = o.asInstanceOf[NeighborWeight]
    if (this.getWeight > n.getWeight) 1
    else if (this.getWeight < n.getWeight) -(1)
    else 0
  }

  def setIndex(_ind: Int): Unit = {
    index = _ind
  }

  def getIndex: Int = index
}
