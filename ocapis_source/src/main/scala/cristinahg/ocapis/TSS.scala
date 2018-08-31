package cristinahg.ocapis

import breeze.linalg.{*, DenseMatrix}
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

class TSS(porcCandidatos:Double=0.01, porcColisiones:Double = 0.01, kEdition:Int = 5) {

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
//    var distanciasEucl = new Array[Array[Double]](normTrainData.length)
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
    //vecinos = vecinos.sorted(_<_)
    //            System.out.print("\n\n\n************************* Inst: "+indInst+" ************************");
    // Eliminamos vecinos hasta que queden solo kEdit vecinos-enemigos
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
    //this.interes = new Array[Double](trainData.length)
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
        //                System.out.print( pesoVecino[j]+",");

        {
          z += 1
          z - 1
        }
      }
      //            System.out.print("\t Suma: "+sumDist);
      // esto se hace para que el peso de un vecino
      // que esté mas cerca sea mayor que el del que está mas lejos
      //             System.out.print("\nPeso Vecinos Normalizando: ");
      var sumaNorm = 0.0
      z = 0
      while ( {
        z < vecinos.size
      }) {
        val neig = vecinos(z).asInstanceOf[Neighbor]
        val clasNeig = neig.classNeig
        pesoVecino(z) = (sumDist - pesoVecino(z)) / sumDist
        sumaNorm = sumaNorm + pesoVecino(z)

        {
          z += 1
          z - 1
        }
      }
      //            System.out.print("\t SumaNorm: "+sumaNorm);
      //            System.out.print("\nPeso Vecinos TRAS Normalizar: ");
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
      //             System.out.print("\t Suma TRAS Norm: "+prueba);
      // Normalizamos el peso de cada vecino entre [0,1]
      // En peso vecinos tengo la importancia de cad vecino segun su distancia
      // PAra calcular el interes de cada muestra, Se multiplican esos
      // pesos por 1 si es un enemigo y por 0 si es un amigo y se suman
      // El mas interesante se da cuando todos son enemigos y estan sobre la
      // instancia (supongamos 5 vecinos considerandos valor maximo.. 5*1=5).
      // habría que normalizar dividiendo entre kEdit
      var suma = 0.0
      //var cuentaVecinosDistintaClase = 0
      z = 0
      while ( {
        z < vecinos.size
      }) {
        val neig = vecinos(z).asInstanceOf[Neighbor]
        val clasNeig = neig.classNeig
        //System.out.print("\n\t ClasIns: "+clasIns+ "  ClasVec: "+clasNeig);
        if (clasNeig != clasIns) {
          suma = suma + pesoVecino(z)
          //System.out.print("\n\t\t Vecino "+j+" peso: "+pesoVecino[j]+ " suma: "+suma);
          //cuentaVecinosDistintaClase += 1
        }

        {
          z += 1
          z - 1
        }
      }
      interes.update(i,suma)
      //System.out.print("\n Interes de instancia "+i+ "  es: "+interes[i]+"  Vecinos Disntitos: "+cuentaVecinosDistintaClase);
      // Habria que comprobar si la instancia es no comparable, en cuyo caso
      // es mas interesante (se le suma 0.5 sin superar el valor de 1)
      //System.out.print("\n ********** DARLE MAS INTERES SI LA INSTANCAS ES NOCOMPARABLE");

      {
        i += 1;
        i - 1
      }
    }
  }

  def CalculaNoDominados(normTrainData: Array[Array[Double]]): Unit = {
//    var noDominados = new Array[Double](normTrainData.length)
//    var Dominados = new Array[Double](normTrainData.length)
//    var pesoDominados = new Array[Double](normTrainData.length)

    var dominado = Array.fill(normTrainData.length){0d}
    val normalizedOutputValues = NormalizeValues(trainlabels)
    for (i <- 0 until normTrainData.length){
      noDominados.append(0d)
      Dominados.append(0d)
      pesoDominados.append(0d)
    }

//    var j = 0
//    while ( {
//      j < normTrainData.length
//    }) {
//      noDominados.append(0d)
//      Dominados.append(0d)
//      pesoDominados.append(0d)
//      {
//        j += 1
//        j - 1
//      }
//    }
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
      //val instX = normTrainData(j)
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
    //
    //return(noComp);
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
      val neigh = instancesCol(i).asInstanceOf[NeighborWeight]
      col = col + neigh.weight

      {
        i += 1;
        i - 1
      }
    }
    val candidatos = (trainData.length * porcCandidatos).asInstanceOf[Int]
    val minColisiones = (col * porcColisiones).asInstanceOf[Int]
    //        System.out.print("\nNúmero de candidatos maximos: "+candidatos+"/"+train.getnData());
    //        System.out.print("\nNúmero de colisiones máximas permitidas: "+minColisiones+"/"+col);
    while ( {
      !theend
    }) {
      instancesCol = calculaColisiones(eliminada)
      var numCol = 0.0
      var i = 0
      while ( {
        i < instancesCol.size
      }) {
        val neigh = instancesCol(i).asInstanceOf[NeighborWeight]
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
        //System.out.print("\n\t---------> Eliminamos Candidato: "+elegido+" con Index: "+eleg.getIndex()+"  col: "+eleg.getWeight());
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
      //            System.out.print("\n Instancia: "+ne.getIndex()+" Dom: "+pesoDominados[i]+"  Inter: "+interes[i]);

      if (pesoDominados(i) < interes(i) || pesoDominados(i) >= 0.9) {

        instancesFinal+=ne
        cont += 1
        //           System.out.print("\t <----   PruebaPeso: "+( pesoDominados[i]-interes[i]));
      }

      {
        i += 1; i - 1
      }
    }
    System.out.print("\n\tSelected: " + cont)
    instancesFinal.toArray
  }


  def execute(traindat: Array[Array[Double]],trainlabs: Array[Double]): Unit = {
    traindat.foreach(i=>{
      val indexOfi=traindat.indexOf(i)
      trainData :+= i
    })

    trainlabs.foreach(l=>trainlabels:+=l)

    val elim = executeSelecColisiones()
    val selec = new Array[Int](elim.length)
    // Primero eliminamos mediante un grasp aquellas que producen colisiones
    // Se permite el que Pueden quedar algunas que provoquen colisiones
    var j = 0
    while ( {
      j < elim.length
    }) {
      if (elim(j) == 0) selec(j) = 1

      {
        j += 1; j - 1
      }
    }
    // actualizamos el conjunto de train solo con las seleccionadas
    val tmp = ArrayBuffer.empty[Array[Double]]
    val tmpOutputs = ArrayBuffer.empty[Double]
    j= 0
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
    var train = tmp
    var labels=tmpOutputs
    //System.out.print("\n Quedan sin colisiones: "+train.getnData());
    val selectedS = executeSelecNoDomin()
    //System.out.print("\n+++++++++++++++++++++++++");
    var S = ArrayBuffer.empty[Array[Double]]
    var i = 0
    while ( {
      i < selectedS.size
    }) {
      val ne = selectedS(i).asInstanceOf[NeighborWeight]
      //System.out.print("\n Se ha elegido: "+ne.getIndex());
      val ins = train(ne.index)
      S.append(ins)

      {
        i += 1; i - 1
      }
    }
    System.out.print("\nAlgorithm Finished.\n")
  }
}

object TSS{
  def main(args: Array[String]): Unit = {
    val tss=new TSS()
    val train=Array(Array(1.0,2.0,3.0,2.0),Array(3.0,6.0,4.0,3.2),Array(4.0,5.3,2.0,7.2))

    val labels=Array(3.0,1.0,3.0)
    tss.execute(train,labels)
  }

}