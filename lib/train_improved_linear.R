#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3

#dat_train <- train.data
#label_train <- train.label
#dat_test <- test.data
#par = par

train <- function(dat_train, label_train){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("BayesTree")
  library("iRF")
  library("rpart")
  library("xgboost")
  
  ### creat model list
  #modelList <- list()
  
  ### Train with gradient boosting model
  #if(is.null(par)){
  #  depth <- 3
  #} else {
  #  depth <- par$depth
  #}
  
  #predArr <- array(NA, c(dim(dat_test)[1], 4, 3))
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    #featMat.test <- dat_test[, , c2]
    
    ## model fitting
    fit_improved <- glm(
      labMat ~ featMat,
      family = gaussian
    )
    #predArr[, c1, c2] <- featMat.test %*% matrix(fit_improved$coefficients[-1], ncol = 1)
    #modelList[[i]] <- list(fit=fit_improved$yhat.test)
  }
  #return(list(predArr = predArr))
  return(fit_improved)
}
