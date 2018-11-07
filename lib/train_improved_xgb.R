#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3

#dat_train <- train.data
#label_train <- train.label
#dat_test <- test.data
#par = par

train <- function(dat_train, label_train,par){
  
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
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    nr <- 100
  } else {
    nr <- par$nr
  }
  
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
    fit_improved <- xgboost(data = as.matrix(featMat), label = as.matrix(labMat), booster = "gblinear", seed = 1,#early_stopping_rounds=10,eta = 0.1,
                                     nrounds = nr, verbose = FALSE, objective = "reg:linear", eval_metric = "rmse", lambda = 1, alpha = 0)
    #predArr[, c1, c2] <- featMat.test %*% matrix(fit_improved$coefficients[-1], ncol = 1)
    #modelList[[i]] <- list(fit=fit_improved)
    #best_iter <- gbm.perf(fit_gbm, method="OOB", plot.it = FALSE)
    modelList[[i]] <- list(fit=fit_improved, iter=par)
  }
  #return(list(predArr = predArr))
  return(modelList)
}
