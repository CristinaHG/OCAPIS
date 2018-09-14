#' K-nearest neighbors for ordinal and monotonic data
#'
#' Predicts labels for test data with ordinal nature or monotonic constraints using an adaptation
#' of wknn.
#' @param traindata Training data of numeric type without labels.
#' @param trainlabels A vector of numeric tags for each instance of training data.
#' @param testdata Test data of numeric type.
#' @param k number of neighbors to use.
#' @param q Minkowski distance param. Use q=1 for Manhattan distance and q=2  for Euclidean distance.
#' @param kerneltype Kernel used to compute neighbors weights. Avaiable kernels are: rectangular, triangular,epanechnikov,
#'biweight, triweight, cosine, gauss and inversion.
#' @param monotonicity Boolean param specifying whether data is monotone or not.
#' @return Predicted labels for test data.
#' @examples
#' dattrain<-read.table("train_balance-scale.0", sep=" ")
#' traindata=dattrain[,-ncol(dattrain)]
#' trainlabels=dattrain[,ncol(dattrain)]
#' testdata<-read.table("test_balance-scale.0", sep=" ")
#' testlabels<-testdata[,ncol(testdata)]
#' testdata<-testdata[,-ncol(testdata)]
#' predictions<-wknnor(traindata,trainlabels,testdata,5,2,"rectangular",FALSE)
#'

wknnor<-function(traindata,trainlabels,testdata,k,q,kerneltype,monotonicity){
  if(nargs()<3) stop("Train Data and labels and TestData must be provided.\n")

  if (length(trainlabels)!= nrow(traindata)){
    stop('Number of train patterns and targets should be the same.\n');
  }
  if(k<0){
    stop("Number of neighbors must be positive.")
  }
  if(!tolower(kerneltype) %in% c("rectangular","triangular","epanechnikov","biweight","triweight","cosine",
                                 "gauss","inversion")){
    stop("Unknown kernel. Avaiable kernels are: rectangular, triangular,epanechnikov,
          biweight, triweight, cosine, gauss and inversion.")
  }

  if(!typeof(monotonicity)=="logical"){
    stop("monotonicity param must be either TRUE or FALSE")
  }
  if (!is.matrix(traindata)){traindata=as.matrix(traindata)}
  if (!is.matrix(testdata)){testdata=as.matrix(testdata)}
  if (! is.integer(trainlabels)){trainlabels<-as.integer(trainlabels)}
  predictions<-s$wknnfit(traindata,trainlabels,testdata,as.integer(k),q,kerneltype,monotonicity)
  predictions
}



