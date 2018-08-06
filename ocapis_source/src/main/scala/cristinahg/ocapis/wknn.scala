package cristinahg.ocapis

import breeze.linalg.functions.minkowskiDistance
import breeze.linalg.{*, DenseMatrix}
import breeze.numerics._
import breeze.numerics.constants._
import breeze.stats.stddev

class wknn {

  def computeWeights(kernelType:String, distances:Array[Double]):Array[Double]={
    kernelType.toUpperCase match {
      case "rectangular" => distances.map(d=>{
        if(abs(d)<=1){
          (1/2)*1
        }else (1/2)*0
      })
      case "triangular" => distances.map(d=>{
        if(abs(d)<=1){
          (1-abs(d))*1
        }else (1-abs(d))*0
      })
      case "epanechnikov" => distances.map(d=>{
        if(abs(d)<=1){
          (3/4)*(1-(d*d))*1
        }else (3/4)*(1-(d*d))*0
      })
      case "biweight" => distances.map(d=>{
        if(abs(d)<=1){
          (15/16)*(1-(d*d))*(1-(d*d))*1
        }else (15/16)*(1-(d*d))*(1-(d*d))*0
      })
      case "triweight" => distances.map(d=>{
        if(abs(d)<=1){
          (35/32)*pow((1-(d*d)),3)*1
        }else (35/32)*pow((1-(d*d)),3)*0
      })
      case "cosine" => distances.map(d=>{
        if(abs(d)<=1){
          (Pi /4)*cos((Pi/2)*d)*1
        }else (Pi /4)*cos((Pi/2)*d)*0
      })
      case "gauss" => distances.map(d=> (1/sqrt(2*Pi))*exp(-((d*d)/2)))
      case "inversion" => distances.map(d=> 1/abs(d))
      case _ => Array(0)
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
    minkowskiDistance()

    val distances=datTest(*,::).map(u=>{
      datTrain(*,::).map(t=>minkowskiDistance(t,u,q)).toArray
    })

    val neightbors=distances.data.map(f=>f.zipWithIndex.sorted.take(k+1))
    val posteriorneightboor=neightbors.map(f=>f.last._1)
    




  }
}
