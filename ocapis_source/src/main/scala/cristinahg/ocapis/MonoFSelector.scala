package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import breeze.numerics.exp
import breeze.linalg.{*, DenseMatrix, DenseVector}


class MonoFSelector {

  private def computeRelation(k: Int, xiVal:Array[Double], xjVal: Array[Double],featureIndex:Int):
  Double={
    1/(1+ exp(k*(xiVal(featureIndex)-xjVal(featureIndex))))
  }

  private def fuzzyMat(datTrain:DenseMatrix[Double],k: Int,featureIndex:Int):DenseMatrix[Double]={
    datTrain(*,::).map(f=>{
      datTrain(*,::).map(r=> computeRelation(k,f.toArray,r.toArray,featureIndex))
    }).toDenseMatrix
  }


  def MonoFSelector(trainData: Array[Array[Double]], trainLabels: Array[Int],k: Int, nSelected: Int):Array[Int]={
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val nfeatures=datTrain.cols
    val fuzzySets= (0 until nfeatures).map(i=>fuzzyMat(datTrain,k,i))

    Array(0)



//      (0 until nfeatures).map(f=>{
//      datTrain(*,::).map(r=>{
//        r mapValues(i=>computeRelation(k,))
//      })
//    })

  }
}

object MonoFSelector {
  val mfs=new MonoFSelector
  val trainDat=Array(Array(1.0,2.0,3.0), Array(5.0,4.0,2.0))
  def main(args: Array[String]): Unit = {
    mfs.MonoFSelector(trainDat,Array(1,2),2,3)
  }
}
