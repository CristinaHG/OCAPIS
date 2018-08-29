package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import cristinahg.ocapis.Neighbor
import java.util
import java.util.Collections

class TSS(porcCandidatos:Double=0.01, porcColisiones:Double = 0.01,kEdition:Int = 5) {

  private val interes = null
  private val pesoDominados = null

  private val noDominados = null
  private val Dominados = null

  private val distanceType = 0
  private val neighbourhood = 0

  private val seed = 0

  private val EUCLIDEAN = 2


  private val INRANGE = 1
  private val OUTOFRANGE = 2

  private val distanciasEucl = null

  import java.util

  private def NormalizeValues(dataValues: Array[Double]): Array[Double] = {
    val min = dataValues.min
    val max = dataValues.max
    dataValues.map(v => (v - min) / (max - min))
  }

  private def calculaColisiones(trainData: Array[Array[Double]],trainlabels:Array[Double], eliminada: Array[Int]) = {
    import java.util
    val conflictos= scala.collection.mutable.MutableList[NeighborWeight]()

    var colisiones = new Array[Int](trainData.length)
    var i = 0
    var ind = 0
    while ( {
      i < trainData.length
    }) colisiones(i) = 0 {
      i += 1;
      i - 1
    }

    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val normalizedInputValues = datTrain(::, *).map(c => NormalizeValues(c.toArray)).inner.toArray
    val normalizedOutputValues=NormalizeValues(trainlabels)
    while ( {
      ind < trainData.length
    }) {
      if (eliminada(ind) == 0) {
        val ins = trainData(ind)
        val insX = normalizedInputValues(ind)
        val outpX = normalizedOutputValues(ind)
        val classX = outpX.toInt
        var y = 0
        while ( {
          y < trainData.length
        }) {
          if (eliminada(y) == 0 && ind != y) {
            val instY = trainData(y)
            val insY = normalizedInputValues(y)
            val outpY = normalizedOutputValues(y)
            val classY = outpY.toInt

            var nonmonotone = true
            var z = 0
            while ( {
              z < insX.length
            }) {
              if (insX(z) > insY(z)) {
                nonmonotone = false
              }

              {
                z += 1;
                z - 1
              }
            }
            if (nonmonotone == true && classX > classY) { // entonces hay conflicto entre indInst e Y
              colisiones(ind) += 1
              colisiones(y) += 1
            }
          }
          {
            y += 1;
            y - 1
          }
        }
      }
      {
        ind += 1;
        ind - 1
      }
    }
    var avgColis = 0

    ind = 0
    while ( {
      ind < trainData.length
    }) {
      if (colisiones(ind) > avgColis && eliminada(ind) == 0) avgColis = colisiones(ind)

      {
        ind += 1;
        ind - 1
      }
    }
    ind = 0
    while ( {
      ind < trainData.length
    }) {
      if (colisiones(ind) > 0) {
        val ne = new NeighborWeight(ind, colisiones(ind))
        conflictos+=(ne)
      }

      {
        ind += 1;
        ind - 1
      }
    }
    conflictos.toArray
  }

  protected def euclideanDistance(instance1: Array[Double], instance2: Array[Double]): Double = {
    var length = 0.0
    var i = 0
    while ( {
      i < instance1.length
    }) {
      length += (instance1(i) - instance2(i)) * (instance1(i) - instance2(i))

      {
        i += 1;
        i - 1
      }
    }
    length = Math.sqrt(length)
    length
  }

  private def calculaDistanciasEuclideas(NormTrainData: Array[Array[Double]]): Unit = {
    var distanciasEucl = new Array[Array[Double]](NormTrainData.length)
    var i = 0
    while ( {
      i < NormTrainData.length
    }) {
      val xiInputs = NormTrainData(i)
      var j = 0
      while ( {
        j < NormTrainData.length
      }) {
        val xjInputs = NormTrainData(j)
        distanciasEucl(i)(j) = euclideanDistance(xiInputs, xjInputs)

        {
          j += 1; j - 1
        }
      }

      {
        i += 1; i - 1
      }
    }
  }



  private def getVecinosMasCercanos(indInst: Int,NormTrainData: Array[Array[Double]],NormLabels:Array[Double]) = {
    var vecinos = scala.collection.mutable.MutableList[Neighbor]()
    val xInput = NormTrainData(indInst)
    val xOutp = NormLabels(indInst)
    val xClass = xOutp.toInt
    var i = 0
    while ( {
      i < NormTrainData.length
    }) {
      if (indInst != i) {
        val neigh = new Neighbor
        val yInput = NormTrainData(i)
        val yOutp = NormLabels(i)
        val yClass = yOutp.toInt
        val dist = distanciasEucl(indInst)(i)
        neigh.distance(dist)
        neigh.index(i)
        neigh.classNeig(yClass)
        vecinos+=neigh
      }

      {
        i += 1; i - 1
      }
    }
    vecinos=vecinos.sorted
    //            System.out.print("\n\n\n************************* Inst: "+indInst+" ************************");
    // Eliminamos vecinos hasta que queden solo kEdit vecinos-enemigos
    while ( {
      vecinos.size > kEdition
    }) vecinos=vecinos.dropRight(1)

    vecinos
  }
}