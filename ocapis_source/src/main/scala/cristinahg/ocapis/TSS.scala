package cristinahg.ocapis

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
    val min=dataValues.min
    val max=dataValues.max
    dataValues.map(v=> (v-min)/(max-min))
  }

  private def calculaColisiones(trainData: Array[Array[Double]], eliminada: Array[Int]) = {
    val conflictos = new util.ArrayList[_]
    val colisiones = new Array[Int](trainData.length)
    var i = 0
    var ind = 0
    while ( {
      i < trainData.length
    }) colisiones(i) = 0 {
      i += 1;
      i - 1
    }

    while ( {
      ind < trainData.length
    }) {
      if (eliminada(ind) == 0) {
        val ins = trainData(ind)
        val insX = ins.getNormalizedInputValues
        val outpX = ins.getNormalizedOutputValues
        val classX = outpX(0).toInt
        var y = 0
        while ( {
          y < trainData.length
        }) {
          if (eliminada(y) == 0 && ind != y) {
            val instY = train.getInstance(y)
            val insY = instY.getNormalizedInputValues
            val outpY = instY.getNormalizedOutputValues
            val classY = outpY(0).toInt

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
    val conColis = 0
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
        conflictos.add(ne)
      }

      {
        ind += 1;
        ind - 1
      }
    }
    conflictos
  }
}