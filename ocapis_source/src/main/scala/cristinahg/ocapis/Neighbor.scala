package cristinahg.ocapis

class Neighbor(var distance:Double = -1.0, var index:Int = -1,var  classNeig:Int = -1) extends Ordered[Any] {

  override def compare(o: Any): Int = {
    val n = o.asInstanceOf[Neighbor]
    if (this.distance > n.distance) 1
    else if (this.distance < n.distance) -(1)
    else 0
  }
}
