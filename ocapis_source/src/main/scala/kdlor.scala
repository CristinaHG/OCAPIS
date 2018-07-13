package cristinahg.ocapis
import breeze.linalg._
import breeze.numerics._
import Numeric._
private class kdlor {

  // TODO: implement QUICKRBF
def computeKernelMatrix(patterns1:  Array[Array[Double]], patterns2:  Array[Array[Double]],kType:String,kParam:Array[Double]) ={
  //pase data from R format to Breeze DenseMatrix
val ncol1=patterns1.length
  val nrow1=patterns1.map(a=>a.length).max
  val ncol2=patterns2.length
  val nrow2=patterns2.map(a=>a.length).max

  var data1=new DenseMatrix(nrow1,ncol1,patterns1.flatten)
  data1=data1.t
  var data2=new DenseMatrix(nrow2,ncol2,patterns2.flatten)
  data2=data2.t

  val Nf1=data1.cols
  val Nf2=data2.cols
  var KM=DenseMatrix.zeros[Double](Nf1,Nf2)
//    val dm = DenseMatrix((1.0,2.0,3.0),
//      (4.0,5.0,6.0))
//    val res = patterns1(::, *) + patterns2(::,1)
//    return res.rows
//    kType.toUpperCase match {
//      case "GAUSS" | "GAUSSIAN"| "RBF" => (0 to Nf2-1).foreach(i=> {
//          val minus=DenseMatrix.ones[Double](1,Nf1)
//          minus:*= exp(-kParam(0))
//        print(minus.data+", ")
//        // KM(::, i) := exp(-kParam(0))*
//          //sum(((patterns1-patterns2(::,i)*DenseMatrix.ones[Double](1,Nf1))^:^2),Axis._0))
//          })
//      case "LINEAR" => KM=(patterns1.t * patterns2)/:/patterns1.rows
//
//      case "POLYNOMIAL" | "POLY" => { var multplusbias=(patterns1.t * patterns2)
//                                      multplusbias:+=1
//                                      KM= (multplusbias/:/patterns1.rows)
//                                      KM:^=kParam(1)
//      }
//      case "SIGMOID" => {
//          if (kParam.length <2) {
//          throw sys.error("Sigmoid kernel needs two parameters")
//          }else (1 to Nf2).foreach(i=> KM(::, i):= tanh(patterns1.t * patterns2(::,i)*kParam(1)*kParam(2)))
//      }
//      case _ =>  throw sys.error("Unknown kernel. Avaiable kernels are: Gauss, Linear, Poly, or Sigmoid.")
//      }
//}
  }

object kdlor extends App {
  var m1=Array(Array(1.0,2.0),Array(3.0,4.0))
  var m2=Array(Array(3.0,4.0),Array(5.0,6.0))
      var computedkernel=computeKernelMatrix(m1,m2,"gauss", Array(1.0,2.0))
}
}

