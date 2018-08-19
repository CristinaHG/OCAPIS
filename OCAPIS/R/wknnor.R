#' K-nearest neighbors for ordinal and monotonic data
#'
#' train data must be the data without labels. Labels should be provided in trainLabels
#' @param x Training data of numeric type without labels.
#' @param y A vector of numeric tags for each instance of training data.
#' @param weights A boolean indicating whether weights per instance are used.
#' @param cost numeric value indicating the cost parameter to train the SVM.
#' @param gamma numeric value indicating the gamma parameter to train the SVM.
#' @return A matrix of 1xn svm trained with weights models.
#' @examples
#' dattrain<-read.csv("train_balance-scale.0", sep=" ")
#' modelstrain<-svmofit(dattrain[,-ncol(dattrain)],dattrain[,ncol(dattrain)],TRUE,1,1)
#'

wknnor<-function(trainData,trainLabels,testData,k,q,kernelType,monotonicity){

}
