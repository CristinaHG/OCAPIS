package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import breeze.numerics.exp

class MonoFSelector {

  private def computeRelation(k: Int, xiVal:Double, xjVal: Double):
  Double={
    1/(1+ exp(k*(xiVal-xjVal)))
  }

  private def fuzzySet(instanceIndex:Int, instanceRelations: Array[Array[Double]], dataInstances: DenseMatrix[Double]):
  Double={

  }

  def MonoFSelector(trainData: Array[Array[Double]], trainLabels: Array[Int],k: Int, nSelected: Int):Array[Int]={
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val nfeatures=datTrain.cols
    val relationsMat=(0 until nfeatures).map(f=>{
      datTrain(*,::).map(r=>{
        r mapValues(i=>computeRelation(k,))
      })
    })

  }
}
