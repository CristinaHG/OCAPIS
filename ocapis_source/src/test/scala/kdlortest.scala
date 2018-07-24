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
  val kd=new kdlor
  val kt="rbf"
  val params=Array(1.0,2.0)
  val optim="qp"
  val fitted=kd.train(trainDataMatrix,labs,kt,params,optim)
//  val projectedTrainfile=io.Source.fromFile(getClass.getResource("/projectedTrain").getPath)
//  var projectedTrainM=ArrayBuffer[Array[Double]]()
//  for (line <- projectedTrainfile.getLines.filterNot(s=>s.startsWith("#"))) {
//    var item=line.split(" ").map(_.trim.toDouble)
//    projectedTrainM+=item
//  }
//  val projectedTrainMatrix=rows.toArray

  val predTrainfile=io.Source.fromFile(getClass.getResource("/predictions").getPath)
  var predictedTrainLabels=ArrayBuffer[Int]()

  for (line <- predTrainfile.getLines.filterNot(s=>s.startsWith("#"))) {
        var item=line.trim.toInt
    predictedTrainLabels+=item
      }
      val predsTrainMat=predictedTrainLabels.toArray
  test("trained method should provide projectedTrainToMatrix data equals to projectedTrainMatrix"){
  assert(fitted(1)===predsTrainMat)
  }

}
