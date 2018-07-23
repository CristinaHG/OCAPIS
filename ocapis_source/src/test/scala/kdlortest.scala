package cristinahg.ocapis

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

class kdlortest extends FunSuite{

  val trainDat=io.Source.fromFile(getClass.getResource("/balance0train").getPath)
  var rows = ArrayBuffer[Array[Double]]()
  for (line <- trainDat.getLines) {
    var item=line.split(" ").map(_.trim.toDouble)
    rows+=item
  }
  val casted=rows.toArray
  val labs=casted.map(u=>u(u.length-1).toInt)
  val trainDataMatrix=casted.map(u=>u.take(u.length-1))
  print(casted(0)(0))

}
