package cristinahg.ocapis

import breeze.linalg.functions.minkowskiDistance
import breeze.linalg.{*, DenseMatrix, DenseVector}
import breeze.numerics._
import breeze.numerics.constants._
import breeze.stats.{stddev,median}

class wknn {

  private def computeWeights(kernelType:String, distances:Array[Double]):Array[Double]={
    kernelType.toLowerCase match {
      case "rectangular" => distances.map(d=>{
        if(abs(d)<=1){
          (1/2)*1.0
        }else (1/2)*0.0
      })
      case "triangular" => distances.map(d=>{
        if(abs(d)<=1){
          (1-abs(d))*1.0
        }else (1-abs(d))*0.0
      })
      case "epanechnikov" => distances.map(d=>{
        if(abs(d)<=1){
          (3/4)*(1-(d*d))*1.0
        }else (3/4)*(1-(d*d))*0.0
      })
      case "biweight" => distances.map(d=>{
        if(abs(d)<=1){
          (15/16)*(1-(d*d))*(1-(d*d))*1.0
        }else (15/16)*(1-(d*d))*(1-(d*d))*0.0
      })
      case "triweight" => distances.map(d=>{
        if(abs(d)<=1){
          (35/32)*pow((1-(d*d)),3)*1.0
        }else (35/32)*pow((1-(d*d)),3)*0.0
      })
      case "cosine" => distances.map(d=>{
        if(abs(d)<=1){
          (Pi /4)*cos((Pi/2)*d)*1.0
        }else (Pi /4)*cos((Pi/2)*d)*0.0
      })
      case "gauss" => distances.map(d=> (1.0/sqrt(2*Pi))*exp(-((d*d)/2)))
      case "inversion" => distances.map(d=> 1.0/abs(d))
      case _ => Array(0.0)
    }
  }

  def fitwknn(trainData:Array[Array[Double]],trainLabels:Array[Int],testData:Array[Array[Double]],k:Int,q:Double,kernelType:String): Unit ={
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTrain = new DenseMatrix(nrowtrain,ncoltrain, trainData.flatten)

    val ncoltest = testData.length
    val nrowtest = testData.take(2).map(a => a.length).max
    val datTest = new DenseMatrix(nrowtest,ncoltest, testData.flatten)

    val standarized=datTrain(::,*).map(c=>c /:/ stddev(c))

    val distances=datTest(*,::).map(u=>{
      standarized(*,::).map(t=>minkowskiDistance(u,t,q)).toArray
    })

    val neightborszipped=distances.data.map(f=>f.zipWithIndex.sorted.take(k+1))
    val neightborsindexes=neightborszipped.map(t=>t.dropRight(1).map(f=>f._2))
    val posteriorsindex=neightborszipped.map(t=>t.last._2)


    val neightbors= neightborsindexes.map(t=>datTrain(t.toSeq,::).toDenseMatrix)
    val posteriors=posteriorsindex.map(t=> datTrain(t,::).inner)


    val distancesToPosterior=(0 until nrowtest).map(i=>{
      minkowskiDistance(datTest(i,::),posteriors(i),q)
    })

    val neightborszippedWithoutposterior=neightborszipped.map(v=>v.dropRight(1))

    val normalizedDistances=neightborszippedWithoutposterior.map(a=>{
      a.map(t=>((t._1/distancesToPosterior(neightborszippedWithoutposterior.indexOf(a)))+0.001,t._2))
    })

    val normalizedDistanceswithoutIndex=normalizedDistances.map(a=>a.map(t=>t._1))
    val normalizedDistancesIndexes=normalizedDistances.map(a=>a.map(t=>t._2))

    val weights=normalizedDistanceswithoutIndex.map(a=>computeWeights(kernelType,a))

    val indexesClass=normalizedDistancesIndexes.map(a=>{
      a.map( b => trainLabels(b))
    })

    val normalizedIndexesWeights=indexesClass.map(a=>{
      val index=indexesClass.indexOf(a)
      (weights(index),a)
    })

    val numClasses = trainLabels.distinct.length

    val predictions=normalizedIndexesWeights.map(a=>{
      val instanceClasses=a._2
      val instanceWeights=a._1
      val probs=(1 to numClasses).map(c=>{
        val filtered=instanceClasses.zipWithIndex.filter(p=>p._1==c)
        filtered.map(f=>instanceWeights(f._2)).sum
      })
      val probsvector=new DenseVector[Double](probs.toArray)
      val medianValue=median(probsvector)
      probs.indexOf(medianValue)+1
    })

  }
}

object wknn{
  def main(args: Array[String]): Unit = {
    val tr=Array(Array(1.0,2.0,3.0),Array(4.0,5.0,2.0))
    val tst=Array(Array(2.0,3.0,4.0),Array(7.0,9.0,5.0))
    val trlabs=Array(1,2,3)
    val k=5
    val q=2.0
    val ktype="gauss"
    val inst=new wknn
    inst.fitwknn(tr,trlabs,tst,k,q,ktype)
  }
}
