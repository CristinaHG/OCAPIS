package cristinahg.ocapis

class Neighbor(var distance:Double = -1.0, var index:Int = -1,var  classNeig:Int = -1) extends Ordered[Neighbor] {


  override def compareTo(that: Neighbor): Int = {
    if (this.distance > that.distance) 1
    else if (this.distance < that.distance) -(1)
    else 0
  }

  override def compare(that: Neighbor): Int = compareTo(that)
}
