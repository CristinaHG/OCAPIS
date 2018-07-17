package cristinahg.ocapis
import breeze.linalg._
import breeze.numerics._
import Numeric._

class kdlor {
 private var projection=DenseMatrix.zeros[Double]
 private var thresholds=DenseMatrix.zeros[Double]
 private var parameters=Array()
 private var kerneltype:String="linear"
  
  // TODO: implement QUICKRBF
  def computeKernelMatrix(patterns1: Array[Array[Double]], patterns2: Array[Array[Double]], kType: String, kParam: Array[Double]) = {
    //parse data from R format to Breeze DenseMatrix
    val ncol1 = patterns1.length
    val nrow1 = patterns1.take(10).map(a => a.length).max
    val ncol2 = patterns2.length
    val nrow2 = patterns2.take(10).map(a => a.length).max

    val dat1 = new DenseMatrix(nrow1, ncol1, patterns1.flatten)
    val data1 = dat1.t
    val dat2 = new DenseMatrix(nrow2, ncol2, patterns2.flatten)
    val data2 = dat2.t

    val Nf1 = data1.cols
    val Nf2 = data2.cols
    var KM = DenseMatrix.zeros[Double](Nf1, Nf2)

    kType.toUpperCase match {
      case "GAUSS" | "GAUSSIAN" | "RBF" => (0 to Nf2 - 1).foreach(i => {
        val diff = (data1(::, *) - data2(::, i)) ^:^ 2.0
        KM(::, i) := exp(-kParam(0) * sum(diff(::, *)).t)
      })
        KM.toArray

      case "LINEAR" => KM = (data1.t * data2) /:/ nrow1.toDouble
        KM.toArray
      case "POLYNOMIAL" | "POLY" => {
        val multplusbias = (data1.t * data2) :+= 1.0
        KM = (multplusbias /:/ nrow1.toDouble) ^:^ kParam(0)
        KM.toArray
      }
      case "SIGMOID" => {
        if (kParam.length < 2) {
          throw sys.error("Sigmoid kernel needs two parameters")
        } else (0 to Nf2 - 1).foreach(i => {
          KM(::, i) := tanh(data1.t * data2(::, i) * kParam(0) + kParam(1))
        })
        KM.toArray
      }
      case _ => throw sys.error("Unknown kernel. Avaiable kernels are: Gauss, Linear, Poly, or Sigmoid.")
    }
  }


  def assignLabels(projected: DenseMatrix[Double],thresholds:DenseMatrix[Double])={
    val numClasses=thresholds.cols+1
    var project2 =tile(projected, 1,numClasses-1)
////    project2=project2 - thresholds.t * DenseMatrix.ones[Double](1,project2.cols)
////    val mapped=project2.mapValues(a=>if(a>0) NaN else a)
////    val maximum=max(mapped(::,*))
////    val rows = mapped(::,*)
////    val indices = maximum.t.map { x =>
////       rows map (a => a.data.indexWhere(_ == x))
////    }
////    var predicted=mapped(::,*).map(a => a.data.indexWhere(_ == maximum))
  }
}

object kdlor{

//  def apply(p1: Array[Array[Double]], p2: Array[Array[Double]], kType: String, kParam: Array[Double]): Unit = {
//    val kd=new kdlor()
//    kd.computeKernelMatrix(p1,p2,kType,kParam)
//  }
////
////  //def main(args: Array[String]): Unit = {
//  def assignlabels(projected: DenseMatrix[Double],thresholds:DenseMatrix[Double])={
//    val kd=new kdlor()
//    //val d1=DenseMatrix((1.0,2.0), (3.0,4.0))
//    //val d2=DenseMatrix((3.0,4.0), (5.0,6.0))
//    kd.assignLabels(projected,thresholds)
//  }
}
//  override def main(args: Array[Any]): Unit = args(0) match {
//    case "kdlor"=> kdlor.computeKernelMatrix(args(1), args(2), args(3), args(4))
//  }



