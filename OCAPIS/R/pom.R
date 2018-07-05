


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
