package cristinahg.ocapis

class NeighborWeight( var index:Int = -1, var weight:Double = .0) extends Ordered[NeighborWeight] {

  override def compareTo(o: NeighborWeight): Int = {
    val n = o.asInstanceOf[NeighborWeight]
    if (this.weight > n.weight) 1
    else if (this.weight < n.weight) -(1)
    else 0
  }

  override def compare(that: NeighborWeight): Int = compareTo(that)
}


