package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import breeze.numerics.abs
import breeze.stats.stddev

class wknn {

  def computeWeights(kernelType:String, distances:Array[Double]):Array[Double]={
    kernelType.toUpperCase match {
      case "rectangular" => distances.map(d=>{
        if(abs(d)<=1){
          (1/2)*1
        }else (1/2)*0
      })
      case   
    }
  }

  def wknn(trainData:Array[Array[Double]],trainLabels:Array[Int],testData:Array[Array[Double]],k:Int,q:Double,kernelType:String): Unit ={
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTrain = new DenseMatrix(nrowtrain,ncoltrain, trainData.flatten)

    val ncoltest = testData.length
    val nrowtest = testData.take(2).map(a => a.length).max
    val datTest = new DenseMatrix(nrowtest,ncoltest, testData.flatten)

    val standarized=stddev(datTrain(::,*))

  }
}
