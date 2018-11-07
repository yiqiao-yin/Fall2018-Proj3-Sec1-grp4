######################################################
### Fit the regression model with testing data ###
######################################################

### Author: Chengliang Tang
### Project 3

#modelList <- fit
#dat_test <- test.data

#fit_improved <- fit_improved
#dat_test <- test.data

test <- function(fit_improved, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model list using training data
  ###  - processed features from testing images
  ### Output: training model specification
  
  ### load libraries
  library("BayesTree")
  library("iRF")
  library("rpart")
  
  predArr <- array(NA, c(dim(dat_test)[1], 4, 3))
  
  
  for (i in 1:12){
    #i = 1
    #fit_train <- modelList[[i]]
    coeff <- fit_improved[[i]]$fit
    ### calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_test[, , c2]
    ### make predictions
    #predArr[, c1, c2] <- predict(fit_train$fit, newdata=data.frame(featMat))
    predArr[, c1, c2] <- featMat %*% matrix(coeff, ncol = 1)
  }
  return(as.numeric(predArr))
}

