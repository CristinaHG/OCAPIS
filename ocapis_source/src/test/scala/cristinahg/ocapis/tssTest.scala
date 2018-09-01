package cristinahg.ocapis

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

class tssTest extends FunSuite  {
  val trainDat = io.Source.fromFile(getClass.getResource("/balance0train").getPath)
  var rows = ArrayBuffer[Array[Double]]()
  for (line <- trainDat.getLines) {
    var item = line.split(" ").map(_.trim.toDouble)
    rows += item
  }
  val casted = rows.toArray
  val labs = casted.map(u => u(u.length - 1))
  val trainDataMatrix = casted.map(u => u.take(u.length - 1))
  val tss = new TSS()

  tss.execute(trainDataMatrix,labs)
}
