package cristinahg.ocapis


import breeze.linalg.{*, DenseMatrix, DenseVector, inv, max, rank, sum, tile}
import breeze.numerics.{exp, isNonfinite, tanh,Inf}
import breeze.optimize.proximal.{ProjectBox, QuadraticMinimizer}
import breeze.stats.mean


class kdlor {

 private var parameters=collection.mutable.Map[String,Double]("u"->0.001,"d"->10)
 private var kerneltype:String="linear"
 private var optimizationMethod="qp"

  // TODO: implement QUICKRBF
  def computeKernelMatrix(data1: DenseMatrix[Double], data2: DenseMatrix[Double], kType: String, kParam: Array[Double]):DenseMatrix[Double] = {
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


  private def assignLabels(projected: DenseMatrix[Double],thresholds:DenseVector[Double]):Array[Int]={
    val numClasses=thresholds.length+1
    var project2 =tile(projected,numClasses-1,1)
    val ones=DenseMatrix.ones[Double](1,project2.cols)
    val right=thresholds*ones
    project2=project2 - right
    //asignation of the class
    val mapped=project2(*,::).map(a=>{
      a.map(b=>if(b>0) -Inf else b)
    })
    //choose the biggest
    val maximum=max(mapped(::,*)).t

    val preds=mapped(::,*).map(a=>a.toArray.indexOf(a.toArray.max)+1)
    var predictions=preds.inner
    //max equals -Inf is because Wx-bk for all k is >0, so this
    //pattern belongs to the last class
    val nanindexes=maximum.findAll(p=>isNonfinite(p))
    nanindexes.foreach(i=> {predictions(i)=numClasses})
    predictions.toArray
  }

  def train(traindat: Array[Array[Double]],trainLabels: Array[Int],kerneltype:String,params:Array[Double]): Array[Array[Double]] ={
    //parse data from R to scala Breeze densematrix format
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

    //var H=CSCMatrix.zeros[Double](dim2,dim2)
    var H=DenseMatrix.zeros[Double](dim2,dim2)
    //Calculate the mean of the classes and the H matrix
    (1 to numClasses).foreach(i=>{
      var currentClass=i
      val range=trainLabels.zipWithIndex.filter(p=>p._1==currentClass).map(a=>a._2).toSeq
      val selections=kernelMatrix(::,range).toDenseMatrix
      val mean1=mean(selections(*,::))
      meanClasses((currentClass-1),::):=mean1.t
      val identity=DenseMatrix.eye[Double](N.getOrElse(currentClass,0))
      val targetsseqclasses=trainLabels.filter(p=>p==currentClass).length.toDouble
      val ones=DenseMatrix.ones[Double](N.getOrElse(currentClass,0),N.getOrElse(currentClass,0))
      H=H+selections*(identity - ones/targetsseqclasses)*(selections.t)
    })

    // Avoid ill-posed matrixes
    var mult=DenseMatrix.eye[Double](dim2)
    val scalar:Double=parameters getOrElse("u", 0.1)
    mult:*=scalar
    H = H +  mult
    val Hinv = inv(H)

    //Calculate the Q matrix for the optimization problem
    (1 until numClasses).foreach(i=>{
      (i to numClasses-1).foreach(j=>{
        Q(i-1,j-1)=(meanClasses(i,::)-meanClasses(i-1,::))*Hinv*(meanClasses(j,::)-meanClasses(j-1,::)).t
        Q(j-1,i-1)=Q(i-1,j-1)
      })
    })
    val vlb = DenseMatrix.zeros[Double](numClasses - 1, 1).toDenseVector //alphas & betas >=0
    val vub = Inf*DenseMatrix.ones[Double](numClasses-1,1).toDenseVector //alphas & betas <=Inf

    //val QtoRmatix=Q(*,::).map(u=>u.data).toArray
//    optimmethod.toLowerCase match {
//      case "qp"=>
        val mrank=rank(Q)
        val qpSolverBounds = new QuadraticMinimizer(mrank, ProjectBox(vlb, vub),E,DenseVector(parameters("d")),2,1e-8)
        val st = qpSolverBounds.initialize
        val alpha=qpSolverBounds.minimize(Q,c.toDenseVector,st)

//      case "cvx"=>
//        val R = org.ddahl.rscala.RClient()
//        R.set("numC",numClasses-1)
//        R.set("Qmatrix",QtoRmatix)
//        val alpha=R.eval(
//          """ require("CVXR")
//              alpha<-Variable(numC)
//              objective <- Minimize(0.5 %*% t(alpha) %*% Qmatrix %*% alpha)
//              problem<-Problem(objective,list((rep(1,numC) %*% alpha==0),alpha>=0))
//              res<-solve(problem)
//              res$getValue(alpha)
//              """
//        )
//    }
    var auxUpdated=aux
    (1 to numClasses -1).foreach(i=>{
      val currentClass=i
      auxUpdated=auxUpdated+alpha(currentClass-1)*(meanClasses(currentClass,::)-meanClasses(currentClass-1,::))
    })

    //projections and thresholds
    val projection = 0.5 * Hinv * auxUpdated.t
    var thresholds = DenseVector.zeros[Double](numClasses -1)
    // thrershold for each pair of classes
    (1 to numClasses -1).foreach(currentclass=>{
      val sumel=(meanClasses(currentclass,::)+meanClasses(currentclass-1,::)).t
      var prod=(projection.t * sumel)
      prod =prod / 2.0
      thresholds(currentclass-1)=prod.data(0)
    })
    var projectedTrain = projection.t * kernelMatrix
    val projectionToMatrix=projectedTrain.toDenseVector.toArray
    // val projectionToMatrix=projection(::,*).map(u=>u.toArray).inner.toArray
    val thresholdsToArray=thresholds.toArray
    val predictedTrain = assignLabels(projectedTrain,thresholds)
    projectedTrain=projectedTrain.t
    //val projectedTrainToMatrix=projectedTrain(::,*).map(u=>u.toArray).inner.toArray
    val projectedTrainToMatrix=projectedTrain.toDenseVector.toArray
    val predictedTrainDouble=predictedTrain.map(t=>t.toDouble)

    Array(projectedTrainToMatrix,predictedTrainDouble,kernelParam,projection.toDenseVector.toArray,thresholdsToArray)
  }

  def predict(trainPatterns:Array[Array[Double]],testPatterns:Array[Array[Double]],kernelType:String,
              kernelP:Array[Double],projection:Array[Double],thres:Array[Double]): Array[Array[Double]] ={
    val ncoltrain = trainPatterns.length
    val nrowtrain = trainPatterns.take(2).map(a => a.length).max
    val datTrain = new DenseMatrix(nrowtrain,ncoltrain, trainPatterns.flatten)

    val ncoltest = testPatterns.length
    val nrowtest = testPatterns.take(2).map(a => a.length).max
    val datTest = new DenseMatrix(nrowtest,ncoltest, testPatterns.flatten)

    //projection to breeze densematrix
    val ncolproj = projection.length
    //val nrowproj = projection.take(2).map(a => a.length).max
    //val datproj = new DenseMatrix(ncolproj, nrowproj, projection.flatten)
    val datproj = new DenseVector(projection)
   // thresholds to breeze densevector
    val threshold=new DenseVector[Double](thres)
    val kernelMat=computeKernelMatrix(datTrain,datTest,kernelType,kernelP)
    val projected=datproj.t* kernelMat
    val predicted=assignLabels(projected.inner.toDenseMatrix,threshold)
    val projectedt=projected.t
    //val projectedtoMat=projectedt(::,*).map(u=>u.toArray).inner.toArray
    val projectedtoMat=projectedt.toArray
    val predictedToDouble=predicted.map(p=>p.toDouble)

    Array(predictedToDouble,projectedtoMat)
  }
}

object kdlor {

  val kd = new kdlor()

  def kdlorfit(data: Array[Array[Double]], datalabels: Array[Int], kernelType: String, params: Array[Double]): Array[Array[Double]] = {
    kd.train(data, datalabels, kernelType, params)
  }

  def kdlorpredict(datatr: Array[Array[Double]],datatst: Array[Array[Double]], kt: String, params: Array[Double],projected:Array[Double],thres:Array[Double]): Array[Array[Double]]= {
    kd.predict(datatr,datatst,kt,params,projected,thres)
  }
}





