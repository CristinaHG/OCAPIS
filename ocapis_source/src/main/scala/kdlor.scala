package cristinahg.ocapis
import breeze.linalg._
import breeze.numerics._
import Numeric._

case object kdlor {

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
        val diff= (data1(::, *) - data2(::, i)*DenseVector.ones[Double](Nf1))^:^2.0
        KM(::, i) :=exp(-kParam(0)*sum(diff(::, *)).t)})
        KM

      case "LINEAR" => KM = (data1.t * data2) /:/ nrow1.toDouble
        KM
      case "POLYNOMIAL" | "POLY" => {
        val multplusbias=(data1.t * data2):+=1.0
        KM= (multplusbias/:/nrow1.toDouble)^:^kParam(0)
        KM
      }
      case "SIGMOID" => {
        if (kParam.length <2) {
          throw sys.error("Sigmoid kernel needs two parameters")
        }else (0 to Nf2-1).foreach(i=> {
          KM(::, i):= tanh(data1.t * data2(::,i)*kParam(0)+kParam(1))})
        KM
      }
      case _ =>  throw sys.error("Unknown kernel. Avaiable kernels are: Gauss, Linear, Poly, or Sigmoid.")
    }
  }
}


//  override def main(args: Array[Any]): Unit = args(0) match {
//    case "kdlor"=> kdlor.computeKernelMatrix(args(1), args(2), args(3), args(4))
//  }



