computeKernel<-function(){
  #print(s$.cristinahg.ocapis.kdlor$computeKernelMatrix())
  s$scala.util.Random$new(123L)
}

#s$.cristinahg.ocapis.kdlor$computeKernelMatrix(as.matrix(dattrain[,-ncol(dattrain)]),as.matrix(dattrain[,-ncol(dattrain)]),5,c(1.0,2.0))

kdlortrain<-function(traindata,trainlabels,kerneltype,c,u,k){
  if(nargs<2) stop("Data and labels must be provided.\n")

  if (length(trainlabels)!= nrow(traindata)){
    stop('Number of patterns and targets should be the same.\n');
  }
  if(!tolower(kerneltype) %in% c("rbf","gauss","gaussian","sigmoid","linear","poly","polynomial")){
    stop("Unknown kernel. Avaiable kernels are: Gauss, Linear, Poly, or Sigmoid.")
  }
}
