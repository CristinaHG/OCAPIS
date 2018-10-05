
#' Train a proportional odd model for ordinal regression
#'
#' train data must be the training data without labels. Labels should be provided in trainLabels.
#' @param train Training data of numeric type without labels.
#' @param trainLabels Tags for each instance of training data. Must be factors.
#' @param linkfunction link function to be used in the ordinal logistic regression fit. Possible functions are: logistic','probit','loglog','cloglog' or 'cauchit'.
#' @return the fitted model.
#' @examples
#' load("./data/train_balance-scale.rda")
#' fit<-pomfit(dattrain[,-ncol(dattrain)],as.factor(dattrain[,ncol(dattrain)]),"logistic")
#'

pomfit<-function(train,trainLabels,linkfunction="logistic"){
  if(!is.factor(trainLabels)) trainLabels<-as.factor(trainLabels)
  setlinkfunc<-function(value){
    if(!(value %in% c("logistic","probit","loglog", "cloglog","cauchit"))) stop("possible linkfunctions are: 'logistic','probit','loglog', 'cloglog' or 'cauchit'")
  }
  setlinkfunc(linkfunction)
  fit<-MASS::polr(trainLabels~.,data=train,Hess = TRUE,method = linkfunction)
  fit
}

#' Predict over the new data instances using the trained model.
#'
#' @param model A trained POM model.
#' @param test Numeric test data without labels.
#' @return A list containing at the first position the projected values per instance per class and at the second position the predicted label for the values.
#' @examples
#' load("./data/train_balance-scale.rda")
#' fit<-pomfit(dattrain[,-ncol(dattrain)],as.factor(dattrain[,ncol(dattrain)]),"logistic")
#' load("./data/test_balance-scale.rda")
#' predictions<-pompredict(fit,dattest[,-ncol(dattest)])
#' projections<-predictions[[1]]
#' predictedLabels<-predictions[[2]]
#'
pompredict<-function(model,test){
  names(test)<-names(model$coefficients)
  preds<-predict(model,test,type="probs")
  predicted<-as.numeric(apply(preds,1,which.max))
  projected<-model$coefficients*test
  list(projected,predicted)
}

