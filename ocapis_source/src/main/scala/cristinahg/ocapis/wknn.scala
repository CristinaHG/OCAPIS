package cristinahg.ocapis

import breeze.linalg.functions.minkowskiDistance
import breeze.linalg.{*, DenseMatrix, DenseVector}
import breeze.numerics._
import breeze.numerics.constants.Pi
import breeze.stats.stddev
import scala.util.Random


class wknn {

  private def computeWeights(kernelType: String, distances: Array[Double]): Array[Double] = {
    kernelType.toLowerCase match {
      case "rectangular" => distances.map ( d =>
        if (abs(d) <= 1)
            .5
        else
          0.0
      )
      case "triangular" => distances.map(d =>
        if (abs(d) <= 1)
          (1.0 - abs(d))
        else
          0.0
      )
      case "epanechnikov" => distances.map(d =>
        if (abs(d) <= 1)
          (3.0 / 4.0) * (1 - (d * d))
         else
          0.0
      )
      case "biweight" => distances.map(d =>
        if (abs(d) <= 1)
          (15.0 / 16.0) * (1 - (d * d)) * (1 - (d * d))
         else
          0.0
      )
      case "triweight" => distances.map(d =>
        if (abs(d) <= 1) {
          (35.0 / 32.0) * pow((1 - (d * d)), 3)
        } else
          0.0
      )
      case "cosine" => distances.map(d =>
        if (abs(d) <= 1)
          (Pi / 4.0) * cos((Pi / 2.0) * d)
        else 0.0
      )
      case "gauss" => distances.map(d => (1.0 / sqrt(2 * Pi)) * exp(-((d * d) / 2.0)))
      case "inversion" => distances.map(d => 1.0 / abs(d))
      case _ => Array(0.0)
    }
  }

  def fitwknn(trainData: Array[Array[Double]], trainLabels: Array[Int], testData: Array[Array[Double]], k: Int, q: Double, kernelType: String,
              monotonicity: Boolean): Array[Int] = {

    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t


    val ncoltest = testData.length
    val nrowtest = testData.take(2).map(a => a.length).max
    val datTst = new DenseMatrix(nrowtest, ncoltest, testData.flatten)
    val datTest = datTst.t
    val standarized = datTrain(::, *).map(c => c /:/ stddev(c))

    val distances = datTest(*, ::).map(u => {
      standarized(*, ::).map(t => minkowskiDistance(u, t, q)).toArray
    })

    val neightborszipped = distances.data.map(f => f.zipWithIndex.sorted.take(k + 1))
    val neightborsindexes = neightborszipped.map(t => t.dropRight(1).map(f => f._2))
    val posteriorsindex = neightborszipped.map(t => t.last._2)

    val neightbors = neightborsindexes.map(t => datTrain(t.toSeq, ::).toDenseMatrix)
    val posteriors = posteriorsindex.map(t => datTrain(t, ::).inner)


    val distancesToPosterior = (0 until datTest.rows).map(i => {
      minkowskiDistance(datTest(i, ::), posteriors(i), q)
    })

    val neightborszippedWithoutposterior = neightborszipped.map(v => v.dropRight(1))

    val normalizedDistances = neightborszippedWithoutposterior.map(a => {
      a.map(t => ((t._1 / distancesToPosterior(neightborszippedWithoutposterior.indexOf(a))) + 0.001, t._2))
    })

    val normalizedDistanceswithoutIndex = normalizedDistances.map(a => a.map(t => t._1))
    val normalizedDistancesIndexes = normalizedDistances.map(a => a.map(t => t._2))

    val indexesClass = normalizedDistancesIndexes.map(a => {
      a.map(b => trainLabels(b))
    })


    if (monotonicity) {
      val classMaxData = datTest(*, ::).map(u => {
        standarized(*, ::).map(x => u.>:=(x).forall(p=>p==true)).toArray
      }).map(d => d.zipWithIndex.filter(d => d._1 == true)).toArray


      val classIndexesForMax = classMaxData.map(f => f.map(i => trainLabels(i._2)))
      val classMaxIndex=classIndexesForMax.map(v=>{
        if(!v.isEmpty) {
          v.max
//          val gruped = v.groupBy(identity)
//          v.groupBy(identity).mapValues(_.length).maxBy(_._2)._1
        } else 1
      })

      val classMinData = datTest(*, ::).map(u => {
        standarized(*, ::).map(x => u.<:=(x).forall(p=>p==true)).toArray
      }).map(d => d.zipWithIndex.filter(d => d._1 == true)).toArray

      val classIndexesForMin = classMinData.map(f => f.map(i => trainLabels(i._2)))

      val classMinIndex=classIndexesForMin.map(v=>{
        if(!v.isEmpty)
          v.min
          //v.groupBy(identity).mapValues(_.length).minBy(_._2)._1
        else 1
      })


      val yMinMax = (0 until classMaxIndex.length).map(i => (classMinIndex(i) to classMaxIndex(i)).toArray)

      val predictions = indexesClass.map(a => {
        val indexOfA = indexesClass.indexOf(a)
        val containedClasses=a.groupBy(identity).mapValues(_.length).filter(p => yMinMax(indexOfA).contains(p._1))

        if(containedClasses.isEmpty)
          Random.shuffle(yMinMax(indexOfA).toList).head
        else
          containedClasses.maxBy(_._2)._1
      })

      predictions

    } else {
      val weights = normalizedDistanceswithoutIndex.map(a => computeWeights(kernelType, a))
      val normalizedIndexesWeights = indexesClass.map(a => {
        val index = indexesClass.indexOf(a)
        (weights(index), a)
      })

      val numClasses = trainLabels.distinct.length

      val predictions = normalizedIndexesWeights.map(a => {
        val instanceClasses = a._2
        val instanceWeights = a._1
        val probs = (1 to numClasses).map(c => {
          val filtered = instanceClasses.zipWithIndex.filter(p => p._1 == c)
          filtered.map(f => instanceWeights(f._2)).sum
        })
        val maxweight=probs.max
        probs.indexOf(maxweight)+1
//        val ponderated = (1 to numClasses).map(u => probs(u - 1) * u)
//        val probsvector = new DenseVector[Double](ponderated.toArray)
//
//        //val medianValue=median(probsvector)
//        val meanValue = mean(probsvector)
//
//        floor(meanValue).toInt + 1
      })
      predictions
    }
  }
}


object wknn{
  val monordwknn = new wknn()

  def wknnfit(trainData: Array[Array[Double]], trainLabels: Array[Int], testData: Array[Array[Double]], k: Int, q: Double, kernelType: String,
               monotonicity: Boolean): Array[Int] ={
    monordwknn.fitwknn(trainData,trainLabels,testData,k,q,kernelType,monotonicity)
  }
}
