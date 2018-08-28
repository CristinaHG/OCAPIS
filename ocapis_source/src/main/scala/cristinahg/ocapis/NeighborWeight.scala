package cristinahg.ocapis

class NeighborWeight( var weight:Double = .0, var index:Int = -1) extends Ordered[Any] {

  override def compare(o: Any): Int = {
    val n = o.asInstanceOf[NeighborWeight]
    if (this.weight > n.weight) 1
    else if (this.weight < n.weight) -(1)
    else 0
  }
}


