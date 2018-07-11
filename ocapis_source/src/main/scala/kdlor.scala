package cristinahg.ocapis
import breeze.linalg._
import breeze.numerics._
private class kdlor {

  def computeKernelMatrix(patterns1: DenseMatrix[Double], patterns2:DenseMatrix[Double],kType:String,kParam:Double)={
    val Nf1=patterns1.cols
    val Nf2=patterns2.cols
    var KM=DenseMatrix.zeros[Double](Nf1,Nf2)

    kType.toUpperCase match {
      case "GAUSS" | "GAUSSIAN"| "RBF" => (1 to Nf2).foreach(i=> KM(::, i) := exp(-kParam*
        sum(
          ((patterns1-patterns2(::,i)*DenseMatrix.ones[Double](1,Nf1)) *:* (patterns1-patterns2(::,i)*DenseMatrix.ones[Double](1,Nf1))).t,Axis._0)))
      
    }
  }
}
