#' Instance Selection for Monotonic Classification
#'
#' Selects the N most relevant instances from ordinal and monotonic dataset, using a three-steps algorithm.
#'
#' @param traindata Training data of numeric type without labels.
#' @param trainlabels A vector of numeric tags for each instance of training data.
#' @param candidates Rate of the best candidates to be selected.
#' @param collisions Minimal rate of collisions permitted to stop the removal process.
#' @param kEdition Maximum number of nearest neighborgs to consider.
#' @return A reduced dataset with the selected instances and its labels.
#'
#' @examples
#' data("train_balance-scale")
#' trainlabels<-dattrain[,ncol(dattrain)]
#' traindata=dattrain[,-ncol(dattrain)]
#' selected<-iselector(traindata,trainlabels,0.01,0.01,5)
#'
#'


iselector<-function(traindata,trainlabels,candidates,collisions,kEdition){
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

  if (kEdition<0){
    stop("K Edition param must be an integer bigger or equal to zero.")
  }
  if (!is.integer(kEdition)){
    kEdition<-as.integer(kEdition)
  }

  if (!is.matrix(traindata)){traindata=as.matrix(traindata)}
  if (!is.double(trainlabels)){
    trainlabels=as.double(trainlabels)
  }
  instanceIndexes<-s$instanceSelec(traindata,trainlabels,candidates,collisions,kEdition)
  instances<-traindata[instanceIndexes,]
  labels<-trainlabels[instanceIndexes]
  cbind(instances,labels)
}
