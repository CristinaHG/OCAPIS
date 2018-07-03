#' compute weights for each instance
#'
#'compute weights for each instance in the data set
#'train data must be in svmlib format
#' @param p A int number.
#' @param tags A vector of tags.
#' @return The weights array for data instances given a value of p.
#'
#' @examples
#' computeWeights(1,c(1,2,3,1,2,1))
#'

computeWeights<-function(p, tags)
  s %!% '
    def computeWeights(p:Double,targets:Array[Int]):Array[Double]={
    val weightscomputed=targets.map(i=>
          if(i<=p) (p + 1 - i*1.0) * (targets.filter(_ <= p).length ) / (targets.filter(_ <= p).map(j=> p+1-j).sum)
          else (i*1.0-p) * (targets.filter(_> p).length) / (targets.filter(_> p).map(j=>j-p).sum))
    return weightscomputed
    }
  computeWeights(p,tags)
  '

#' trains the model for the SVMOP method with trainning data and given parameters
#' train data must be in svmlib format
#' @param x A number.
#' @param y A vector of tags.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
#'
svmofit<-function(train,trainLabels,weights=TRUE){
  classes<-unique(trainLabels)
  nOfClasses = length(classes)
  models<-matrix(list(), 1, nOfClasses -1)
  for (i in 2:nOfClasses){
      train_labels<-rep(0,length(trainLabels))
      train_labels[which(trainLabels<i)]=-1
      train_labels[which(trainLabels>=i)]<-1

      # compute instances weights
      if(weights){
        weightsTrain=computeWeights(i-1,trainLabels)
      }else weightsTrain=rep(1,length(trainLabels))
      # train
      models[[1,i-1]]<-weighted.ksvm(y=train_labels, x=train,weights=weightsTrain,kernel = "rbfdot",prob.model=TRUE)
      if(is.atomic(models[[1,i-1]])){
          warning("Empty model. Please check the training patterns.")
      }
  }
  models
}

svmopredict<-function(models,test){
  projected<-matrix(0,length(models)+1,nrow(test))
  for(i in 2:(length(models)+1)){
    probs<-kernlab::predict(models[[1,i-1]],kernlab::as.kernelMatrix(as.matrix(test[,-ncol(test)])),type = "probabilities")

  }



}
#0.9555077 0.9768768 0.9792856 0.9876574 0.9853661
#dattrain<-read.csv("train_balance-scale.0", sep=" ")
#modelstrain<-svmofit(dattrain[,-ncol(dattrain)],dattrain[,ncol(dattrain)],TRUE)
#dattest<-read.csv("test_balance-scale.0", sep=" ")


