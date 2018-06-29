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
svmopfit<-function(train,trainLabels,weights=TRUE){
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
      models[[1,i]]<-weighted.ksvm(y=train_labels, x=train,weights=weightsTrain,kernel = "rbfdot")
      if(is.empty.model(models[[1,i]])){
          warning("Empty model. Please check the training patterns.")
      }
  }
  models
}

svmtrain<-function(x){

}

#modelstrain<-svmopfit(dattrain[,-ncol(dattrain)],dattrain[,ncol(dattrain)],TRUE)
