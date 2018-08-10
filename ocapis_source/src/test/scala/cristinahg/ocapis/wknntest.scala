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
  val testDataMatrix = castedtst.map(u => u.take(u.length - 1))
  val wknn = new wknn
  val k=4
  val q=2.0
  val ktype="epanechnikov"
  val fitted = wknn.fitwknn(trainDataMatrix,labs,testDataMatrix,k,q,ktype)

  val errors=(0 until fitted.length).map(i=>{
    if(fitted(i)==labstst(i)) 0 else 1
  }).sum

  test("predicted labels for test data should equal labstst"){
    assert(fitted==labstst)
  }
}
