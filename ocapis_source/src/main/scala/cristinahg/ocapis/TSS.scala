package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TSS(var porcCandidatos:Double=0.01, var porcColisiones:Double = 0.01, var kEdition:Int = 5) {

  private val interes=scala.collection.mutable.MutableList[Double]()
  private val pesoDominados=ArrayBuffer.empty[Double]

  private val noDominados = ArrayBuffer.empty[Double]
  private val Dominados = ArrayBuffer.empty[Double]

  private val distanceType = 0

  private val seed = 0

  private var distanciasEucl = ArrayBuffer.empty[Array[Double]]
  private var trainData = ArrayBuffer.empty[Array[Double]].toArray
  private var trainlabels = ArrayBuffer.empty[Double].toArray

  private def NormalizeValues(dataValues: Array[Double]): Array[Double] = {
    val min = dataValues.min
    val max = dataValues.max
    dataValues.map(v => (v - min) / (max - min))
  }

  private def calculaColisiones(eliminada: Array[Int]): Array[NeighborWeight] = {
    val conflictos = scala.collection.mutable.MutableList[NeighborWeight]()

    var colisiones = Array.fill(trainData.length){0}
    var ind = 0

    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val normalizedcols = datTrain(::, *).map(c => NormalizeValues(c.toArray)).inner.toArray
    val normalizedInputValues=normalizedcols.transpose
    val normalizedOutputValues = NormalizeValues(trainlabels)
    while ( {
      ind < trainData.length
    }) {
      if (eliminada(ind) == 0) {
        val insX = normalizedInputValues(ind)
        val outpX = normalizedOutputValues(ind)
        val classX = outpX.toInt
        var y = 0
        while ( {
          y < trainData.length
        }) {
          if (eliminada(y) == 0 && ind != y) {
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
                z += 1
                z - 1
              }
            }
            if (nonmonotone && classX > classY) { // entonces hay conflicto entre indInst e Y
              colisiones(ind) += 1
              colisiones(y) += 1
            }
          }
          {
            y += 1
            y - 1
          }
        }
      }
      {
        ind += 1
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
        ind += 1
        ind - 1
      }
    }
    ind = 0
    while ( {
      ind < trainData.length
    }) {
      if (colisiones(ind) > 0) {
        val ne = new NeighborWeight(ind, colisiones(ind))
        conflictos += ne
      }

      {
        ind += 1
        ind - 1
      }
    }
    conflictos.toList.toArray
  }

  protected def euclideanDistance(instance1: Array[Double], instance2: Array[Double]): Double = {
    var length = 0.0
    var i = 0
    while ( {
      i < instance1.length
    }) {
      length += (instance1(i) - instance2(i)) * (instance1(i) - instance2(i))

      {
        i += 1
        i - 1
      }
    }
    length = Math.sqrt(length)
    length
  }

  private def calculaDistanciasEuclideas(normTrainData: Array[Array[Double]]): Unit = {
    for (i <- 0 until normTrainData.length)
      distanciasEucl.append(Array.fill(normTrainData.length)(0d))
    var i = 0
    while ( {
      i < normTrainData.length
    }) {
      val xiInputs = normTrainData(i)
      var j = 0
      while ( {
        j < normTrainData.length
      }) {
        val xjInputs = normTrainData(j)
        distanciasEucl(i)(j) = euclideanDistance(xiInputs, xjInputs)

        {
          j += 1;
          j - 1
        }
      }

      {
        i += 1;
        i - 1
      }
    }
  }


  private def getVecinosMasCercanos(indInst: Int, NormTrainData: Array[Array[Double]], NormLabels: Array[Double]) = {
    var vecinos = ArrayBuffer.empty[Neighbor]
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
        neigh.distance=dist
        neigh.index=i
        neigh.classNeig=yClass
        vecinos.append(neigh)
      }

      {
        i += 1;
        i - 1
      }
    }
    vecinos.toArray.sorted

    while ( {
      vecinos.size > kEdition
    }) vecinos = vecinos.dropRight(1)

    vecinos
  }


  private def CalculaInteres(): Unit = {

    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t

    val normalizedcols = datTrain(::, *).map(c => NormalizeValues(c.toArray)).inner.toArray
    val normalizedInputValues=normalizedcols.transpose
    val normalizedOutputValues = NormalizeValues(trainlabels)

    calculaDistanciasEuclideas(normalizedInputValues)
    var i = 0
    while ( {
      i < trainData.length
    }) {
      interes+=0
      val insOutp = normalizedOutputValues(i)
      val clasIns = insOutp.toInt
      val vecinos = getVecinosMasCercanos(i, normalizedInputValues, normalizedOutputValues)
      var pesoVecino = new Array[Double](vecinos.size)
      // sumo las distancias de la instancia i hasta todos sus vecinos
      var sumDist = 0.0

      var z = 0
      while ( {
        z < vecinos.size
      }) {
        val neig = vecinos(z)
        sumDist += neig.distance
        pesoVecino(z) = neig.distance

        {
          z += 1
          z - 1
        }
      }

      var sumaNorm = 0.0
      z = 0
      while ( {
        z < vecinos.size
      }) {
        val neig = vecinos(z)
        val clasNeig = neig.classNeig
        pesoVecino(z) = (sumDist - pesoVecino(z)) / sumDist
        sumaNorm = sumaNorm + pesoVecino(z)

        {
          z += 1
          z - 1
        }
      }

      var prueba = 0.0
      z = 0
      while ( {
        z < vecinos.size
      }) {
        pesoVecino(z) = pesoVecino(z) / sumaNorm
        prueba = prueba + pesoVecino(z)

        {
          z += 1
          z - 1
        }
      }

      var suma = 0.0
      z = 0
      while ( {
        z < vecinos.size
      }) {
        val neig = vecinos(z)
        val clasNeig = neig.classNeig
        if (clasNeig != clasIns) {
          suma = suma + pesoVecino(z)
        }

        {
          z += 1
          z - 1
        }
      }
      interes.update(i,suma)

      {
        i += 1;
        i - 1
      }
    }
  }

  def CalculaNoDominados(normTrainData: Array[Array[Double]]): Unit = {
    var dominado = Array.fill(normTrainData.length){0d}
    val normalizedOutputValues = NormalizeValues(trainlabels)
    for (i <- 0 until normTrainData.length){
      noDominados.append(0d)
      Dominados.append(0d)
      pesoDominados.append(0d)
    }

    val instPerClas = new Array[Int](trainlabels.distinct.length)
    var j = 0
    while ( {
      j < normTrainData.length
    }) {
      val insX = normTrainData(j)
      val outpX = normalizedOutputValues(j)
      val clasX = outpX.toInt
      instPerClas(clasX) += 1
      var y = 0
      while ( {
        y < normTrainData.length
      }) {
        val insY = normTrainData(y)
        val outpY = normalizedOutputValues(y)
        if (j != y) {
          var menoresoIguales = 0
          var z = 0
          while ( {
            z < insX.length
          }) {
            if (insX(z) <= insY(z)) menoresoIguales += 1

            {
              z += 1;
              z - 1
            }
          }

          if (menoresoIguales == insX.length && outpX == outpY) {
            dominado(j) = dominado(j) + 1
          }
        }

        {
          y += 1;
          y - 1
        }
      }

      {
        j += 1;
        j - 1
      }
    }
    j = 0
    while ( {
      j < normTrainData.length
    }) {
      val outpX = normalizedOutputValues(j)
      val clasX = outpX.toInt
      Dominados.update(j,dominado(j))
      noDominados.update(j,instPerClas(clasX) - dominado(j))

      val temp = Math.abs(Dominados(j) - noDominados(j))
      pesoDominados.update(j,temp / (Dominados(j) + noDominados(j)))

      {
        j += 1;
        j - 1
      }
    }
  }

  def executeSelecColisiones():
  Array[Int] = {
    var theend = false
    var eliminada = Array.fill(trainData.length){0}

    // Calculo el número inidical de colisiones del dataset
    var instancesCol = calculaColisiones(eliminada)

    var col = 0.0
    var i = 0
    while ( {
      i < instancesCol.length
    }) {
      val neigh = instancesCol(i)
      col = col + neigh.weight

      {
        i += 1;
        i - 1
      }
    }
    val candidatos = (trainData.length * porcCandidatos).asInstanceOf[Int]
    val minColisiones = (col * porcColisiones).toInt
    while ( {
      !theend
    }) {
      instancesCol = calculaColisiones(eliminada)
      var numCol = 0.0
      var i = 0
      while ( {
        i < instancesCol.size
      }) {
        val neigh = instancesCol(i)
        numCol = numCol + neigh.weight

        {
          i += 1;
          i - 1
        }
      }
      if (instancesCol.size > 0) { // si hay colisiones
        instancesCol = instancesCol.sorted.reverse
        // se elige un candidato de entre los primeros 'candidatos'
        var elegido = -1
        if (instancesCol.size < candidatos) elegido = Random.nextInt( (instancesCol.size) + 1)
        else elegido = Random.nextInt( (candidatos) + 1 )
        // Cogemos 'elegido' de la lista de candidatos
        val eleg = instancesCol(elegido)
        eliminada(eleg.index) = 1
      }
      if (numCol <= minColisiones) theend = true
    }
    eliminada
  }



  def executeSelecNoDomin(): Array[NeighborWeight] = {
    val theend = false
    val ncoltrain = trainData.length
    val nrowtrain = trainData.take(2).map(a => a.length).max
    val datTr = new DenseMatrix(nrowtrain, ncoltrain, trainData.flatten)
    val datTrain = datTr.t
    var instancesFinal=scala.collection.mutable.MutableList[NeighborWeight]()
    val normalizedcols = datTrain(::, *).map(c => NormalizeValues(c.toArray)).inner.toArray
    val normTrainData=normalizedcols.transpose
    val selec = Array.fill(normTrainData.length){1}

    // calculamos el peso de las NoDominadas
    CalculaNoDominados(normTrainData)
    // calculamos el interes al principio
    calculaDistanciasEuclideas(normTrainData)
    CalculaInteres()
    var cont = 0
    var i = 0
    while ( {
      i < trainData.length
    }) {
      val ne = new NeighborWeight(i, 0)

      if (pesoDominados(i) < interes(i) || pesoDominados(i) >= 0.9) {

        instancesFinal+=ne
        cont += 1
      }

      {
        i += 1; i - 1
      }
    }
    instancesFinal.toArray
  }


  def execute(traindat: Array[Array[Double]],trainlabs: Array[Double],cand:Double=0.01, col:Double=0.01, kEd:Int=5): Unit = {
    traindat.foreach(i=>{
      val indexOfi=traindat.indexOf(i)
      trainData :+= i
    })

    trainlabs.foreach(l=>trainlabels:+=l)

    porcCandidatos=cand
    porcColisiones=col
    kEdition=kEd

    val elim = executeSelecColisiones()
    val selec = elim.map(e=>if(e==0) 1 else 0)
    // actualizamos el conjunto de train solo con las seleccionadas
    val tmp = ArrayBuffer.empty[Array[Double]]
    val tmpOutputs = ArrayBuffer.empty[Double]
    var j= 0
    while ( {
      j < trainData.length
    }) {
      if (selec(j) == 1) {
        val ins = trainData(j)
        val lab=trainlabels(j)
        tmp.append(ins)
        tmpOutputs.append(lab)

      }

      {
        j += 1; j - 1
      }
    }
     trainData = tmp.toArray
     trainlabels=tmpOutputs.toArray
    val selectedS = executeSelecNoDomin()
    var S = ArrayBuffer.empty[Array[Double]]
    var classS=ArrayBuffer.empty[Double]
    var i = 0
    while ( {
      i < selectedS.size
    }) {
      val ne = selectedS(i)
      val ins = trainData(ne.index)
      val cls=trainlabels(ne.index)
      S.append(ins)
      classS.append(cls)

      {
        i += 1; i - 1
      }
    }
    Array(S.toArray,classS.toArray)
  }
}

object TSS {
  val tss = new TSS

  def instanceSelec(traindat: Array[Array[Double]], trainlabs: Array[Double], cand: Double = 0.01, col: Double = 0.01, kEd: Int = 5): Unit = {
    tss.execute(traindat, trainlabs, cand, col, kEd)
  }
}


