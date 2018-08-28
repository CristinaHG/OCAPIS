package cristinahg.ocapis

class Neighbor(var distance:Double = -1.0, var index:Int = -1,var  classNeig:Int = -1) extends Comparable[_] {

  def this(_dist: Double, _ind: Int) {
    this()
    distance = _dist
    index = _ind
    classNeig = -1
  }

  def this(_dist: Double, _ind: Int, _claNeig: Int) {
    this()
    distance = -1
    index = -1
    classNeig = _claNeig
  }

  def getDistance: Double = distance

  def setDistance(_dist: Double): Unit = {
    distance = _dist
  }

  override def compareTo(o: Any): Int = {
    val n = o.asInstanceOf[Neighbor]
    if (this.getDistance > n.getDistance) 1
    else if (this.getDistance < n.getDistance) -(1)
    else 0
  }

  def setIndex(_ind: Int): Unit = {
    index = _ind
  }

  def getIndex: Int = index

  def getClassNeig: Int = classNeig

  def setClassNeig(classNeig: Int): Unit = {
    this.classNeig = classNeig
  }
}
