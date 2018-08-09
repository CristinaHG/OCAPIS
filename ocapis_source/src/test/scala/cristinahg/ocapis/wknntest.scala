package cristinahg.ocapis

import scala.collection.mutable.ArrayBuffer
import org.scalatest._

class wknntest extends FunSuite{
  val trainDat = io.Source.fromFile(getClass.getResource("/balance0train").getPath)
  var rows = ArrayBuffer[Array[Double]]()
  for (line <- trainDat.getLines) {
    var item = line.split(" ").map(_.trim.toDouble)
    rows += item
  }
  val casted = rows.toArray
  val labs = casted.map(u => u(u.length - 1).toInt)
  val trainDataMatrix = casted.map(u => u.take(u.length - 1))

  val testDat = io.Source.fromFile(getClass.getResource("/balance0test").getPath)
  rows.clear()
  for (line <- testDat.getLines) {
    var item = line.split(" ").map(_.trim.toDouble)
    rows += item
  }
  val castedtst = rows.toArray
  val labstst = castedtst.map(u => u(u.length - 1).toInt)
  val testDataMatrix = casted.map(u => u.take(u.length - 1))
  val wknn = new wknn
  val k=4
  val q=2.0
  val ktype="gauss"
  val fitted = wknn.fitwknn(trainDataMatrix,labs,testDataMatrix,k,q,ktype)
  
}
