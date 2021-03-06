########################
### Cross Validation ###
########################

### Author: Chengliang Tang
### Project 3

#X.train <- feat_train
#y.train <- label_train
#d <- model_values[k]
#K

cv.function <- function(X.train, y.train, d, K,
                        n.trees = 200, bag.fraction = 0.5, inter.depth = 10
                        ){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  ### create cluster object
  require(parallel)
  cl <- makeCluster(12)
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    par <- list(depth=d)
    fit <- train(train.data, train.label, par,
                 n.trees = n.trees, bag.fraction = bag.fraction, inter.depth = inter.depth)
    pred <- test(fit, test.data)
    cv.error[i] <- mean((pred - test.label)^2)
  }
  ### close
  stopCluster(cl)
  return(c(mean(cv.error),sd(cv.error)))
}
