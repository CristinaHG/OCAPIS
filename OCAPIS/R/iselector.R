


iselector<-function(traindata,trainlabels,candidates,collisions,kEdition){
  #traindat: Array[Array[Double]], trainlabs: Array[Double], cand: Double = 0.01, col: Double = 0.01, kEd: Int = 5
  if(nargs()<2) stop("At least Train Data and labels must be provided.\n")

  if (length(trainlabels)!= nrow(traindata)){
    stop('Number of train patterns and targets should be the same.\n');
  }
  if(candidates<0){
    stop("Percentage of candidates must be positive.")
  }
  if(collisions<0){
    stop("Percentage of allowed collisions must be 0 or positive.")
  }

  if (kEdition<0 || !is.integer(kEdition)){
    stop("K Edition param must be an integer bigger or equal to zero.")
  }

  if (!is.matrix(traindata)){traindata=as.matrix(traindata)}

  selected<-s$iselector(traindata,trainlabels,candidates,collisions,as.integer(kEdition))
  instances<-selected(0L)
  labels<-selected(1L)
  cbind(instances,labels)
}
