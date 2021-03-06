% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/kdlor.R
\name{kdlortrain}
\alias{kdlortrain}
\title{Trains a KDLOR model}
\usage{
kdlortrain(traindata, trainlabels, kernel, d, u, k)
}
\arguments{
\item{traindata}{Data to train kdlor model. Tags should not be provided in traindata.}

\item{trainlabels}{Class labels for training data. Must be numeric of type integer.}

\item{kernel}{Type of kernel to compute the Gram matrix. One of "rbf","gauss","gaussian","sigmoid","linear","poly","polynomial".}

\item{d}{Numeric parameter for optimization method. Default is 10.}

\item{u}{Numeric parameter for H matrix computation. Default is 0.01.}

\item{k}{Array of kernel Params. If kernel type is sigmoid, Array of two values should be provided.}
}
\value{
An instance of kdlorModel class containing the fields: projectedTrain, predictedTrain, kerneltype, kernelParam, projection and thresholds

where:

projectedTrain is the projected matrix for training data.

predictedTrain are the predicted numeric labels for training data.

kerneltype is the used kernel type for the fit. Should be used again for prediction.

kernelParam is the numeric kernel param used for computing the kernel matrix.

projection is the general projected matrix that should be used in predict.

thresholds is an array of doubles representing the model thresholds to be used in prediction.

Each of these fields can be accesed with "@" (see section examples) below.
}
\description{
Trains the Kernel Discriminant Learning for Ordinal Regression model with training data
}
\examples{
# read train data
data("train_balance-scale")
traindata=dattrain[,-ncol(dattrain)]
trainlabels=dattrain[,ncol(dattrain)]
# fit the kdlor model
myfit<-kdlortrain(traindata,trainlabels,"rbf",10,0.001,1)
# acess kdlor model fields
myfit@predictedTrain


}
