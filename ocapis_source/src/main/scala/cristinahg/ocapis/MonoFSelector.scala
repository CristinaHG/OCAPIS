package cristinahg.ocapis

import breeze.numerics.{Inf, ceil, exp, log}
import breeze.linalg.{*, DenseMatrix, DenseVector}


class MonoFSelector {
  private val decimals=2
  private def computeRelation(k: Int, xiVal:Array[Double], xjVal: Array[Double],featureIndex:Int):
  Double={
    1.0/(1.0+ exp(k*(xiVal(featureIndex)-xjVal(featureIndex))))
  }

  private def fuzzyMat(datTrain:DenseMatrix[Double],k: Int,featureIndex:Int):Array[Array[Double]]={
    datTrain(*,::).map(f=>{
      datTrain(*,::).map(r=> computeRelation(k,f.toArray,r.toArray,featureIndex)).toArray
    }).toArray
  }


  private def RMI(ordSetsA1:Array[Array[Float]],ordSetsA2:Array[Array[Float]],n:Int):Double={
    val infosum=(1 to n).map(i=>{
      val ordS1=ordSetsA1(i-1).toSet
      val ordS2=ordSetsA2(i-1).toSet
      (1.0/i)*log((ordS1.size * ordS2.size)/ (n*(ordS1.intersect(ordS2).size)))
    }).sum
    -infosum
  }

  private def relevance(ordSetsA1:Array[Array[Float]],ordSetsA2:Array[Float],n:Int):Double={
    val ordS2=ordSetsA2.toSet
    val infosum=(1 to n).map(i=>{
      val ordS1=ordSetsA1(i-1).toSet
      val intersection=(n*(ordS1.intersect(ordS2).size))
      intersection match {
        case 0 => (1.0/i)*Inf
        case _ => (1.0/i)*log((ordS1.size * ordS2.size)/ intersection )
      }
    }).sum
    -infosum
  }

  private def roundValue(value: Double): Float = {
      val mult = Math.pow(10, decimals)
      return (value * mult.round / mult).toFloat
  }

  def MonoFSelector(trainData: Array[Array[Double]], trainLabels: Array[Int],k: Int,beta:Double, nSelected: Int):Array[Int]={
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val nfeatures = datTrain.cols
    val fuzzyMats = (0 until nfeatures).map(i=>fuzzyMat(datTrain,k,i))


    val ordsets= fuzzyMats.map(m =>{
        m.map(f=>{
          f.map(i=>{
            val colIndex=f.indexOf(i)
            i/datTrain(colIndex,::).inner
          }).map(d=>d.toArray).transpose.map(_.sum).map(v=>roundValue(v))
      })
    })


    val fuzzyDecission=trainLabels.map(i=>{
      trainLabels.map(j=>(1.0/(1.0+ exp(k*(i.toDouble-j.toDouble)))))
    })

    val ordsetDecision=fuzzyDecission.map(r=>{
      r.map(e=>{
        val instanceIndex=r.indexOf(e)
        e/datTrain(instanceIndex,::).inner
      }).map(d=>d.toArray).transpose.map(_.sum).map(v=>roundValue(v))
    })


    val frmi=(0 until nfeatures).map(i=>{
      val relev=relevance(ordsets(i),ordsetDecision(i),datTrain.rows)
      val redundancy=ordsets.map(s=>RMI(ordsets(i),s,datTrain.rows)).sum
      (0.5*relev-(beta/4.0)*redundancy)
    }).zipWithIndex

    val maxFrmi=frmi.sortBy(_._1).takeRight(nfeatures)
    val bestFeatures=maxFrmi.map(f=>f._2).toArray
    bestFeatures



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
    mfs.MonoFSelector(trainDat,Array(1,2),2,2.0,3)
  }
}
