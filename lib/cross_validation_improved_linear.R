########################
### Cross Validation ###
########################

### Author: Chengliang Tang
### Project 3

#X.train <- feat_train
#y.train <- label_train
#d <- model_values[k]
#K

cv.function <- function(X.train, y.train,K){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    #predArr <- array(NA, c(dim(test.data)[1], 4, 3))
    
    # fit linear model
    fit_improved <- train(train.data, train.label)
    pred <- test(fit_improved,test.data)
    cv.error[i] <- mean((pred - test.label)^2)
  }
  return(c(mean(cv.error),sd(cv.error)))
}
