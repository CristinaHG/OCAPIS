package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import breeze.numerics.{exp, log}
import breeze.linalg.{*, DenseMatrix, DenseVector}


class MonoFSelector {

  private def computeRelation(k: Int, xiVal:Array[Double], xjVal: Array[Double],featureIndex:Int):
  Double={
    1/(1+ exp(k*(xiVal(featureIndex)-xjVal(featureIndex))))
  }

  private def fuzzyMat(datTrain:DenseMatrix[Double],k: Int,featureIndex:Int):Array[Array[Double]]={
    datTrain(*,::).map(f=>{
      datTrain(*,::).map(r=> computeRelation(k,f.toArray,r.toArray,featureIndex)).toArray
    }).toArray
  }

  private def RMI(ordSetA1:Array[Double],ordSetA2:Array[Double],beta:Double,n:Int):Double={
    val ordS1=ordSetA1.toSet
    val ordS2=ordSetA2.toSet
    val infosum=(1 to n).map(i=>{
                (1.0/i)*log((ordS1.size * ordS2.size)/ (n*(ordS1.intersect(ordS2).size)))
                }).sum
    -infosum
  }

  def MonoFSelector(trainData: Array[Array[Double]], trainLabels: Array[Int],k: Int, nSelected: Int):Array[Int]={
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val nfeatures = datTrain.cols
    val fuzzyMats = (0 until nfeatures).map(i=>fuzzyMat(datTrain,k,i))

    val fuzzy0=fuzzyMats(0)

    val ordsets= fuzzyMats.map(m =>{
        m.map(f=>{
          f.map(i=>{
            val colIndex=f.indexOf(i)
            i/datTrain(colIndex,::).inner
          }).map(d=>d.toArray).transpose.map(_.sum)
      })
    })

    //ordsets.map(i=>i.toSet)

   // val summed=ordsetsmr



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
