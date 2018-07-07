
#' Train a proportional odd model for ordinal regression
#'
#' train data must be the training data without labels. Labels should be provided in trainLabels.
#' @param x Training data of numeric type without labels.
#' @param y Tags for each instance of training data. Must be factors.
#' @param linkfunction link function to be used in the ordinal logistic regression fit. Possible functions are: logistic','probit','loglog','cloglog' or 'cauchit'.
#' @return the fitted model.
#' @examples
#' dattrain<-read.csv("train_balance-scale.0", sep=" ")
#' fit<-pomfit(dattrain[,-ncol(dattrain)],as.factor(dattrain[,ncol(dattrain)]),"logistic")
#'

pomfit<-function(train,trainLabels,linkfunction="logistic"){
  setlinkfunc<-function(value){
    if(!(value %in% c("logistic","probit","log-log","cauchit"))) stop("possible linkfunctions are: 'logistic','probit','loglog','cloglog' or 'cauchit'")
  }
  setlinkfunc(linkfunction)
  fit<-polr(trainLabels~.,data=train,Hess = TRUE,method = linkfunction)
  fit
}

# fit<-pomfit(dattrain[,-ncol(dattrain)],as.factor(dattrain[,ncol(dattrain)]),"logistic")

pompredict<-function(model,test){
  names(test)<-names(model$coefficients)
  preds<-predict(model,test,type="probs")
  predicted<-as.numeric(apply(model$fitted.values,1,which.max))
  projected<-model$coefficients*test
  c(projected,predicted)
}

#fit$coefficients*dattest[,-ncol(dattest)]
#identical(predict(fit,dattrain[,-ncol(dattrain)],type="probs"),fit$fitted.values)
