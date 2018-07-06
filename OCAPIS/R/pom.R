


pomfit<-function(train,trainLabels,linkfunction="logistic"){
  setlinkfunc<-function(value){
    if(!(value %in% c("logistic","probit","log-log","cauchit"))) stop("possible linkfunctions are: 'logistic','probit','loglog','cloglog' or 'cauchit'")
  }
  setlinkfunc(linkfunction)
  fit<-polr(trainLabels~.,data=train,Hess = TRUE,method = linkfunction)
  #thresholds=-fit$zeta
  #projection<-fit$coefficients
  fit
}

# fit<-polr(as.factor(dattrain[,ncol(dattrain)])~.,data=dattrain[,-ncol(dattrain)],Hess = TRUE,method ="logistic")
#myfit<-pomfit(dattrain[,-ncol(dattrain)],as.factor(dattrain[,ncol(dattrain)]),"logistic")

pompredict<-function(model,test){
  predicted<-as.numeric(apply(model$fitted.values,1,which.max))
  projected<-fit$coefficients*test
  c(projected,predicted)
}
fit$coefficients*dattest[,-ncol(dattest)]
