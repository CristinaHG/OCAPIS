


pomfit<-function(train,trainLabels,linkfunction="logistic"){
  setlinkfunc<-function(value){
    if(!(value %in% c("logistic","probit","log-log","cauchit"))) stop("possible linkfunctions are: 'logistic','probit','loglog','cloglog' or 'cauchit'")
  }
  if(!is.matrix(train)){
    train<-as.matrix(train)
  }
  setlinkfunc(linkfunction)
  fit<-polr(trainLabels~.,data=train,Hess = TRUE,method = linkfunction)
  fit
}

# fit<-polr(as.factor(dattrain[,ncol(dattrain)])~.,data=dattrain[,-ncol(dattrain)],Hess = TRUE,method ="logistic")
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
