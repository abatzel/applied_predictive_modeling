## #############################################################################
## 
## APPLIED PREDICTIVE MODELING, CH.7: NONLINEAR ESTIMATORS
## 
## AUTHOR: Nat Henry
## CREATED: May 14, 2019
## PURPOSE: Show examples for algorithms in APM Ch. 7
## 
## #############################################################################

rm(list=ls())

## Load packages
library(caret) # For KNN example
library(ggplot2)
library(mda) # For MARS example
library(Metrics) # Get RMSE
library(neuralnet) # For NN example


## 
## ** KNN CODE EXAMPLE **
## 

# NOTE: This example was taken from the `caret` documentation
data(BloodBrain)

#' Data info:
#'  Mente and Lombardo (2005) develop models to predict the log of the ratio of 
#'  the concentration of a compound in the brain and the concentration in blood. 
#'  For each compound, they computed three sets of molecular descriptors: MOE 2D, 
#'  rule-of-five and Charge Polar Surface Area (CPSA). In all, 134 descriptors 
#'  were calculated. Included in this package are 208 non-proprietary literature 
#'  compounds. The vector logBBB contains the concentration ratio and the data 
#'  frame bbbDescr contains the descriptor values.

inTrain <- createDataPartition(logBBB, p = .8)[[1]]

trainX <- bbbDescr[inTrain,]
trainY <- logBBB[inTrain]

testX <- bbbDescr[-inTrain,]
testY <- logBBB[-inTrain]

fit <- knnreg(trainX, trainY, k = 3, weights = "distance")

knn_plot_train <- ggplot() + 
  geom_point(aes(x=trainY, y=predict(fit, trainX))) +
  geom_abline(intercept=0, slope=1, color='red')
plot(knn_plot_train)
message(sprintf(
  "Training RMSE for KNN: %s", rmse(trainY, predict(fit, trainX))
))
  
knn_plot_test <- ggplot() +
  geom_point(aes(x=testY, y=predict(fit, testX))) +
  geom_abline(intercept=0, slope=1, color='red')
plot(knn_plot_test)
message(sprintf(
  "Testing RMSE for KNN: %s", rmse(testY, predict(fit, testX))
))



## 
## ** MARS CODE EXAMPLE **
## 

data(trees)
fit1 <- mda::mars(trees[,-3], trees[3])
showcuts <- function(obj)
{
  tmp <- obj$cuts[obj$sel, ]
  dimnames(tmp) <- list(NULL, names(trees)[-3])
  tmp
}
showcuts(fit1)

## examine the fitted functions
par(mfrow=c(1,2), pty="s")
Xp <- matrix(sapply(trees[1:2], mean), nrow(trees), 2, byrow=TRUE)
for(i in 1:2) {
  xr <- sapply(trees, range)
  Xp1 <- Xp; Xp1[,i] <- seq(xr[1,i], xr[2,i], len=nrow(trees))
  Xf <- predict(fit1, Xp1)
  plot(Xp1[ ,i], Xf, xlab=names(trees)[i], ylab="", type="l")
}


## 
## ** NEURAL NETWORK CODE EXAMPLE **
## 

## EXAMPLE 1
data(infert, package="datasets")
net.infert <- neuralnet(
  case~parity+induced+spontaneous, infert,
  err.fct="ce", linear.output=FALSE, likelihood=TRUE
)
gwplot(net.infert, selected.covariate="parity")
gwplot(net.infert, selected.covariate="induced")
gwplot(net.infert, selected.covariate="spontaneous")
confidence.interval(net.infert)
plot(net.infert)



## EXAMPLE 2
Var1 <- rpois(100,0.5)
Var2 <- rbinom(100,2,0.6)
Var3 <- rbinom(100,1,0.5)
SUM <- as.integer(abs(Var1+Var2+Var3+(rnorm(100))))
sum.data <- data.frame(Var1,Var2,Var3, SUM)
net.sum <- neuralnet(
  SUM~Var1+Var2+Var3, sum.data, hidden=1,
  act.fct="tanh"
)
prediction(net.sum)

