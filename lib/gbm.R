############## GBM: GRADIENT BOOSTING MACHINE ######################

# NOTE
# This script separate train/test relying on 
# regression() function which uses the cutoff
# parameter to cut the data set into train and test.
# For ex, a cutoff of 0.9 means that the first 90% of observations
# are used as training and the last 10% of observations 
# are used as validating.

# This is more efficient because train and test are 
# both in one function.

# Define function:
grad.boost.machine <- function(
  all = all,
  cutoff = 0.9) {
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  train <- data.frame(Y = train.y, X = train.x)
  
  # Model Fitting
  ### load libraries
  library("gbm")
  model <- gbm(Y~.,             # formula
               data=train,                   # dataset
               #var.monotone=c(0,0,0,0,0,0), # -1: monotone decrease,
               # +1: monotone increase,
               #  0: no monotone restrictions
               distribution="gaussian",     # see the help for other choices
               n.trees=200,                # number of trees
               shrinkage=0.05,              # shrinkage or learning rate,
               # 0.001 to 0.1 usually work
               interaction.depth=3,         # 1: additive model, 2: two-way interactions, etc.
               bag.fraction = 0.5,          # subsampling fraction, 0.5 is probably best
               train.fraction = 0.5,        # fraction of data for training,
               # first train.fraction*N used for training
               #mFeatures = 3,               # half of the features are considered at each node
               n.minobsinnode = 10,         # minimum total weight needed in each node
               cv.folds = 3,                # do 3-fold cross-validation
               keep.data=TRUE,              # keep a copy of the dataset with the object
               verbose=FALSE               # don't print out progress
  )         
  best_iter <- gbm.perf(model, method="OOB", plot.it = FALSE)
  #summary(model)
  
  # Training result
  preds.train <- predict(model, train[, -1],
                         n.trees=best_iter, type="response")
  preds.train <- data.frame(preds.train)
  mse.train <- mean((preds.train- train.y)^2)
  
  # Make prediction:
  colnames(test.x) <- c(as.character(colnames(train[,-1])))
  preds <- predict(model, data.frame(test.x),
                   n.trees=best_iter, type="response")
  preds <- data.frame(preds)
  mse <- mean((preds - test.y)^2)
  
  # Final output:
  return(
    list(
      Summary = list(fit=model, iter=best_iter[1]),
      Training.MSE= mse.train,
      Testing.MSE = mse
    )
  )
} # End of function

