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

computeWeights<-function(p,tags)
  s %!% '
    def computeWeights(p:Double,targets:Array[Int]):Array[Double]={
    val weightscomputed=targets.map(i=>
          if(i<=p) (p + 1 - i*1.0) * (targets.filter(_ <= p).length ) / (targets.filter(_ <= p).map(j=> p+1-j).sum)
          else (i*1.0-p) * (targets.filter(_> p).length) / (targets.filter(_> p).map(j=>j-p).sum))
    return weightscomputed
    }
  computeWeights(p,tags)
  '

#' Trains n-1 SVM models for ordinal data with given parameters
#'
#' train data must be the data without labels. Labels should be provided in trainLabels
#' @param x Trainning data of numeric type without labels.
#' @param y A vector of numeric tags for each instance of trainning data.
#' @param weights A boolean indicating whether weights per instance are used.
#' @param cost numeric value indicating the cost parameter to train the SVM.
#' @param gamma numeric value indicating the gamma parameter to train the SVM.
#' @return A matrix of 1xn svm trained with weights models.
#' @examples
#' dattrain<-read.csv("train_balance-scale.0", sep=" ")
#' modelstrain<-svmofit(dattrain[,-ncol(dattrain)],dattrain[,ncol(dattrain)],TRUE,1,1)
#'
svmofit<-function(train,trainLabels,weights=TRUE,cost,gamma){
  mysvm<-import_from_path("svmutil",system.file("python","python",package = "OCAPIS"))
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
      #models[[1,i-1]]<-weighted.ksvm(y=train_labels, x=train,weights=weightsTrain,kernel = "rbfdot",prob.model=TRUE)
      parameters<-paste0('-b 1',' ','-t 2',' ',paste0('-c ',cost),' ', paste0('-g ',gamma),' -q')
      param<-mysvm$svm_parameter(r_to_py(parameters))
      # remeber to remove the class from dattrain
      problem<-mysvm$svm_problem(r_to_py(weightsTrain),r_to_py(train_labels),r_to_py(dattrain)$values$tolist())
      models[[1,i-1]]<-mysvm$svm_train(problem,param)
      if(is.atomic(models[[1,i-1]])){
          warning("Empty model. Please check the training patterns.")
      }
  }
  models
}

#test without labels
svmopredict<-function(models,test){
  mysvm<-import_from_path("svmutil",system.file("python","python",package = "OCAPIS"))
  projected<-matrix(0,length(models)+1,nrow(test))
  for(i in 2:(length(models)+1)){
    #pred<-kernlab::predict(models[[1,i-1]],kernlab::as.kernelMatrix(as.matrix(test[,-ncol(test)])),type = "probabilities")
    pred<-mysvm$svm_predict(r_to_py(rep(0,nrow(test))),r_to_py(test)$values$tolist(),models[[1,i-1]],r_to_py('-b 1'))
    predprob<-pred[[3]]
    projected[i-1,]<-unlist(predprob[seq(2,length(predprob), by=2)])
    #projected[i-1,]<-matrix(unlist(pred[[3]]),ncol=2)[,2]
  }
  probts<-matrix(0,length(models)+1,nrow(test))
  probts[1,]<-rep(1,length(projected[1,]))-projected[1,]
  for(i in 2:(length(models)+1)){
    probts[i,]<-projected[i-1,]-projected[i,]
  }
  probts[length(models)+1,]<-projected[length(models),]
  predicted<-apply(probts,2,max)

  c(projected,predicted)
}


#0.9555077 0.9768768 0.9792856 0.9876574 0.9853661
#dattrain<-read.csv("train_balance-scale.0", sep=" ")
#modelstrain<-svmofit(dattrain[,-ncol(dattrain)],dattrain[,ncol(dattrain)],TRUE,1,1)
#dattest<-read.csv("test_balance-scale.0", sep=" ")
#pred<-mysvm$svm_predict(r_to_py(dattest$X1),r_to_py(dattest[,-ncol(dattest)])$values$tolist(),modelstrain[[1,1]],r_to_py('-b 1')

