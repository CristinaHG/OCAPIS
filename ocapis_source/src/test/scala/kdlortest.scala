package cristinahg.ocapis

import org.scalatest.FunSuite

import scala.collection.mutable.ArrayBuffer

class kdlortest extends FunSuite {

  val trainDat = io.Source.fromFile(getClass.getResource("/balance0train").getPath)
  var rows = ArrayBuffer[Array[Double]]()
  for (line <- trainDat.getLines) {
    var item = line.split(" ").map(_.trim.toDouble)
    rows += item
  }
  val casted = rows.toArray
  val labs = casted.map(u => u(u.length - 1).toInt)
  val trainDataMatrix = casted.map(u => u.take(u.length - 1))
  val kd = new kdlor
  val kt = "rbf"
  val params = Array(1.0, 2.0)
  val optim = "qp"
  val fitted = kd.train(trainDataMatrix, labs, kt, params)
  val project = fitted(3) match {
    case x: Array[Double] => x
    case _ => throw new RuntimeException("Expected projection matrix as Array[Double]")
  }

  val thres = fitted(4) match {
    case x: Array[Double] => x
    case _ => throw new RuntimeException("Expected thresholds vector as Array[Double]")
  }
  val predicted = kd.predict(trainDataMatrix, trainDataMatrix, kt, params, project, thres)
  //  val projectedTrainfile=io.Source.fromFile(getClass.getResource("/projectedTrain").getPath)
  //  var projectedTrainM=ArrayBuffer[Array[Double]]()
  //  for (line <- projectedTrainfile.getLines.filterNot(s=>s.startsWith("#"))) {
  //    var item=line.split(" ").map(_.trim.toDouble)
  //    projectedTrainM+=item
  //  }
  //  val projectedTrainMatrix=rows.toArray

  val predTrainfile = io.Source.fromFile(getClass.getResource("/predictions").getPath)
  var predictedTrainLabels = ArrayBuffer[Int]()

  for (line <- predTrainfile.getLines.filterNot(s => s.startsWith("#"))) {
    var item = line.trim.toInt
    predictedTrainLabels += item
  }
  val predsTrainMat = predictedTrainLabels.toArray
  test("train method should provide projectedTrainToMatrix data equals to projectedTrainMatrix") {
    assert(fitted(1) === predsTrainMat)
  }

  test("test method should provide labeled data equal to projectedTrainMatrix") {
    assert(fitted(1) === predsTrainMat)
  }
}
