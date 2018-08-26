fselector<-function(traindata,trainlabels,k,beta,nselected){
  if(nargs()<3) stop("Train Data and labels and number of features to select must be provided.\n")

  if (length(trainlabels)!= nrow(traindata)){
    stop('Number of train patterns and targets should be the same.\n');
  }
  if(k<0){
    stop("k constant must be positive.")
  }
  if(!is.numeric(beta)){
    stop("Regularization param must be numeric.")
  }

  if(nselected<1){
    stop("number of features to select must be a positive integer.")
  }
  if (!is.matrix(traindata)){traindata=as.matrix(traindata)}

  selected<-s$fselector(traindata,trainlabels,k,beta,as.integer(nselected))
  selected
}
