
setClass(Class="kdlorModel",
         representation(
           projectedTrain="numeric",
           predictedTrain="numeric",
           kerneltype="character",
           kernelParam="numeric",
           projection="numeric",
           thresholds="numeric"
         ),
         prototype(projectedTrain = NA_real_, predictedTrain= NA_real_,kerneltype=NA_character_,
                   kernelParam=NA_real_,projection=NA_real_,thresholds=NA_real_))


#' Trains a KDLOR model
#'
#'Trains the Kernel Discriminant Learning for Ordinal Regression model with training data
#' @param traindata Data in matrix format. Tags should not be provided in traindata.
#' @param trainlabels Class labels for training data. Must be numeric of type integer.
#' @param kernel Type of kernel to compute the Gram matrix. One of "rbf","gauss","gaussian","sigmoid","linear","poly","polynomial".
#' @param c Numeric parameter for optimization method. Default is 10.
#' @param u Numeric parameter for H matrix computation. Default is 0.01.
#' @param k Array of kernel Params. If kernel type is sigmoid, Array of two values should be provided.
#' @return List(projectedTrain,predictedTrain,kerneltype,kernelParam,projection,thresholds)
#'
#' where:
#'
#' projectedTrain is the projected matrix for training data.
#'
#' predictedTrain are the predicted numeric labels for training data.
#'
#' kerneltype is the used kernel type for the fit. Should be used again for prediction.
#'
#' kernelParam is the numeric kernel param used for computing the kernel matrix.
#'
#' projection is the general projected matrix that should be used in predict.
#'
#' thresholds is an array of doubles representing the model thresholds to be used in prediction.
#'
#' @examples
#' computeWeights(1,c(1,2,3,1,2,1))
#'
#'
kdlortrain<-function(traindata,trainlabels,kernel,d,u,k){
  if(nargs()<2) stop("Data and labels must be provided.\n")

  if (length(trainlabels)!= nrow(traindata)){
    stop('Number of patterns and targets should be the same.\n');
  }
  if(!tolower(kernel) %in% c("rbf","gauss","gaussian","sigmoid","linear","poly","polynomial")){
    stop("Unknown kernel. Avaiable kernels are: Gauss, Linear, Poly, or Sigmoid.")
  }
  if(!typeof(d)=="double" || !typeof(u)=="double" || !typeof(k)=="double"){
    stop("d, u and k params must be of type numeric.")
  }
  if (!is.matrix(traindata)){traindata=as.matrix(traindata)}
  params=c()
  if (length(c(d,u,k))==3){
    params<-c(params,d)
    params<-c(params,u)
    params<-c(params,k)
  }
  myfit<-s$kdlorfit(traindata,trainlabels,kernel,params)
  #create R object - kdlor Model
  (new ("kdlorModel",
         projectedTrain=myfit(0L),
         predictedTrain=myfit(1L),
         kerneltype=kernel,
         kernelParam=myfit(2L),
         projection=myfit(3L),
         thresholds=myfit(4L)
  ))
}

#dattrain<-read.csv("train_balance-scale.0", sep=" ")
#traindata=dattrain[,-ncol(dattrain)]
#trainlabels=dattrain[,ncol(dattrain)]
#myfit<-kdlortrain(traindata,trainlabels,"rbf",10,0.001,1)

