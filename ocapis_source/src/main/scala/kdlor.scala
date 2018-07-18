package cristinahg.ocapis
import breeze.linalg._
import breeze.numerics._
import breeze.stats.{hist,mean}
import Numeric._

class kdlor {
 // var projection:DenseMatrix[Double]
 // var thresholds:DenseMatrix[Double]
 private var parameters=collection.mutable.Map("u"->0.001,"d"->10)
 private var kerneltype:String="linear"
 private var optimizationMethod="quadprog"

  // TODO: implement QUICKRBF
  def computeKernelMatrix(data1: DenseMatrix[Double], data2: DenseMatrix[Double], kType: String, kParam: Array[Double]):DenseMatrix[Double] = {
    //parse data from R format to Breeze DenseMatrix
//    val ncol1 = patterns1.length
//    val nrow1 = patterns1.take(2).map(a => a.length).max
//    val ncol2 = patterns2.length
//    val nrow2 = patterns2.take(2).map(a => a.length).max
//
//    val dat1 = new DenseMatrix(nrow1, ncol1, patterns1.flatten)
//    val data1 = dat1.t
//    val dat2 = new DenseMatrix(nrow2, ncol2, patterns2.flatten)
//    val data2 = dat2.t

    val Nf1 = data1.cols
    val Nf2 = data2.cols
    val nrow1 =data1.rows
    var KM = DenseMatrix.zeros[Double](Nf1, Nf2)

    kType.toUpperCase match {
      case "GAUSS" | "GAUSSIAN" | "RBF" => (0 to Nf2 - 1).foreach(i => {
        val diff = (data1(::, *) - data2(::, i)) ^:^ 2.0
        KM(::, i) := exp(-kParam(0) * sum(diff(::, *)).t)
      })
        KM

      case "LINEAR" => KM = (data1.t * data2) /:/ nrow1.toDouble
        KM
      case "POLYNOMIAL" | "POLY" => {
        val multplusbias = (data1.t * data2) :+= 1.0
        KM = (multplusbias /:/ nrow1.toDouble) ^:^ kParam(0)
        KM
      }
      case "SIGMOID" => {
        if (kParam.length < 2) {
          throw sys.error("Sigmoid kernel needs two parameters")
        } else (0 to Nf2 - 1).foreach(i => {
          KM(::, i) := tanh(data1.t * data2(::, i) * kParam(0) + kParam(1))
        })
        KM
      }
      case _ =>  KM
        // throw sys.error("Unknown kernel. Avaiable kernels are: Gauss, Linear, Poly, or Sigmoid.")

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

  def train(traindat: Array[Array[Double]],trainLabels: Array[Int],kerneltype:String,params:Array[Double]): Unit ={
    //parse data from R to scala format
    val ncol1 = traindat.length
    val nrow1 = traindat.take(2).map(a => a.length).max
    val dat1 = new DenseMatrix(nrow1, ncol1, traindat.flatten)
    //val data1 = dat1.t
    val dim= dat1.rows
    val numTrain=dat1.cols
    var kernelParam=Array(1.0)
    if(params.length==3){
      parameters("d")=params(0)
      parameters("u")=params(1)
      kernelParam=Array(params(2))
    }else{
      kerneltype.toLowerCase match {
        case "rbf" |"gauss" | "gaussian" => kernelParam = Array(1.0)
        case "sigmoid" =>  kernelParam=Array(1.0,2.0)
        case "linear" => kernelParam = Array(1.0)
      }
    }

    // Compute the Kernel matrix
    val kernelMatrix = computeKernelMatrix(dat1,dat1,kerneltype, kernelParam)
    val dim2 = numTrain
    val numClasses = trainLabels.distinct.length
    var meanClasses = DenseMatrix.zeros[Double](numClasses,dim2)
    val Q=DenseMatrix.zeros[Double](numClasses-1, numClasses-1)
    val c = DenseMatrix.zeros[Double](numClasses - 1, 1)
    var A=DenseMatrix.ones[Double](numClasses-1,numClasses-1)
    A=(-A)
    val b=DenseMatrix.zeros[Double](numClasses - 1, 1)
    val E=DenseMatrix.ones[Double](1,numClasses-1)
    val aux=DenseMatrix.zeros[Double](1,dim2)
    //val N=hist(DenseVector(trainLabels),DenseVector((1 to numClasses).toArray))
    val N=trainLabels.groupBy(a=>a).map(x=>(x._1->x._2.size))

    var H=CSCMatrix.zeros[Double](dim2,dim2)

    //Calculate the mean of the classes and the H matrix
    (1 to numClasses).foreach(i=>{
      var currentClass=i
      val range=trainLabels.zipWithIndex.filter(p=>p._1==currentClass).map(a=>a._2).toSeq
      val selections=kernelMatrix(::,range).toDenseMatrix
      val mean1=mean(selections(*,::))
      meanClasses((currentClass-1),::):=mean1.t
      val identity=DenseMatrix.eye[Double](N(currentClass-1))
      val targetsseqclasses=trainLabels.filter(p=>p==currentClass).sum.toDouble
      var km=kernelMatrix(::,range).toDenseMatrix.t
      km*=targetsseqclasses

      //H=H+selections*identity - DenseMatrix.ones(N(1,currentClass),N(1,currentClass))/denom
    })
  }
}

object kdlor{

//  def apply(p1: Array[Array[Double]], p2: Array[Array[Double]], kType: String, kParam: Array[Double]): Unit = {
//    val kd=new kdlor()
//    kd.computeKernelMatrix(p1,p2,kType,kParam)
//  }
////
def main(args: Array[String]): Unit = {
val kd=new kdlor()
val d1=Array(Array(1.0,2.0), Array(3.0,4.0),Array(5.0,6.0),Array(7.0,2.0))
val d2=DenseMatrix((3.0,4.0), (5.0,6.0))
val labels=Array(1,2,1,1)
    kd.train(d1,labels,"rbf",Array(1,2))
  }
}
//  override def main(args: Array[Any]): Unit = args(0) match {
//    case "kdlor"=> kdlor.computeKernelMatrix(args(1), args(2), args(3), args(4))
//  }



