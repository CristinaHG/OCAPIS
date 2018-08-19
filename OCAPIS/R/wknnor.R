#' K-nearest neighbors for ordinal and monotonic data
#'
#' Predicts labels for test data with ordinal nature or monotonic constraints using an adaptation
#' of wknn.
#' @param trainData Training data of numeric type without labels.
#' @param trainLabels A vector of numeric tags for each instance of training data.
#' @param testData Test data of numeric type.
#' @param q Minkowski distance param.Use q=1 for Manhattan distance and q=2  for Euclidean distance.
#' @param kernelType Kernel used to compute neighbors weights. Avaiable kernels are: rectangular, triangular,epanechnikov,
#'biweight, triweight, cosine, gauss and inversion.
#' @param monotonicity Boolean param specifying whether data is monotone or not.
#' @return Predicted labels for test data.
#' @examples
#' dattrain<-read.csv("train_balance-scale.0", sep=" ")
#' traindata=dattrain[,-ncol(dattrain)]
#' trainlabels=dattrain[,ncol(dattrain)]
#' testdata<-read.csv("test_balance-scale.0", sep=" ")
#' testdata<-testdata[,-ncol(testdata)]
#' testlabels<-svmofit(traindata,trainlabels,testdata,5,2,"rectangular",FALSE)
#'

wknnor<-function(trainData,trainLabels,testData,k,q,kernelType,monotonicity){

}


