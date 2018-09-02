//package cristinahg.ocapis
//
//import org.scalatest.FunSuite
//
//import scala.collection.mutable.ArrayBuffer
//
//class tssTest extends FunSuite  {
//
//  val trainDat = io.Source.fromFile(getClass.getResource("/balance0train").getPath)
//  var rows = ArrayBuffer[Array[Double]]()
//  for (line <- trainDat.getLines) {
//    var item = line.split(" ").map(_.trim.toDouble)
//    rows += item
//  }
//  val casted = rows.toArray
//  val labs = casted.map(u => u(u.length - 1))
//  val trainDataMatrix = casted.map(u => u.take(u.length - 1))
//  val tss = new TSS()
//
//  val execution=tss.execute(trainDataMatrix,labs)
//
//
//  val tssouts=io.Source.fromFile(getClass.getResource("/tssoutputs").getPath)
//  val tsslabs=io.Source.fromFile(getClass.getResource("/tssoutputslabs").getPath)
//  var tssrows = ArrayBuffer[Array[Double]]()
//  for (line <- tssouts.getLines) {
//    var item = line.split(" ").map(_.trim.toDouble)
//    tssrows += item
//  }
//  var selectedlabs=ArrayBuffer[Double]()
//  for (line <- tsslabs.getLines) {
//    var item = line.trim.toDouble
//    selectedlabs+=item
//  }
//  test("TSS execution selected instances should equal tssrows"){
//    assert(tssrows.toArray===execution(0))
//  }
//
//  test("TSS execution selected instances labels should equal selectedlabs"){
//    assert(selectedlabs.toArray===execution(1))
//  }
//}
