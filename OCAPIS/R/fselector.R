#' Feature Selection for Monotonic Classification
#'
#' Selects the N most relevant features from ordinal and monotonic data, based on mRMR criterion.
#'
#' @param traindata Training data of numeric type without labels.
#' @param trainlabels A vector of numeric tags for each instance of training data.
#' @param k positive constant for logit function. If large, fuzzy ordinal set is understood as slightly
#' larger. If small, is understood as significantly larger.
#' @param beta Regulation param for relative importance of MI between features and decision.
#' @param nselected Number of features to select.
#' @return nselected most important features.
#' @examples
#' dattrain<-read.csv("train_balance-scale.0", sep=" ")
#' traindata=dattrain[,-ncol(dattrain)]
#' selected<-fselector(traindata,trainlabels,2,2,2)
#'
#'
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

  if (nselected<1){
    stop("number of features to select must be a positive integer.")
  }
  if (!is.integer(k)) nselected=as.integer(k)
  if (!is.integer(nselected)) nselected=as.integer(nselected)

  if (!is.matrix(traindata)){traindata=as.matrix(traindata)}

  selected<-s$fselector(traindata,trainlabels,k,beta,as.nselected)
  selected
}
