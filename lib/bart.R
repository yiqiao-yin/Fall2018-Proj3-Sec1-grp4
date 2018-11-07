######################## BAYESTREE: BAYESIAN ADDITIVE REGRESSION TREE ####################

# Library
library("BayesTree"); library("pROC")

# Define function:
bayesian.additive.regression.tree <- function(
  all = all,
  cutoff = .9,
  num.tree = 5,
  cutoff.coefficient = .9
) {
  
  # Split data:
  train <- all[1:(cutoff*nrow(all)),]; dim(train) # Training set
  test <- all[(cutoff*nrow(all)+1):nrow(all),]; dim(test) # Testing set
  
  # Identify Response and Explanatory:
  train.x <- train[,-1]; dim(train.x)
  train.y <- train[,1]; head(train.y)
  test.x <- test[,-1]; dim(test.x)
  test.y <- test[,1]; dim(data.frame(test.y))
  
  # Modeling fitting:
  model <- bart(
    x.train = train.x,
    y.train = train.y,
    x.test  = test.x,
    verbose = FALSE,
    ntree = num.tree)
  sum <- summary(model)
  
  # Make prediction on training:
  preds.train.prob <- colMeans(model$yhat.train)
  preds.train <- preds.train.prob
  mse.train <- mean((preds.train- train.y)^2)

  # Make prediction on testing:
  preds.prob <- colMeans(model$yhat.test)
  preds <- preds.prob
  mse <- mean((preds - test.y)^2)
  
  # Final output:
  return(
    list(
      #Summary = sum,
      Training.MSE= mse.train,
      Testing.MSE = mse
    )
  )
} # End of function

# Run
#model.result <- bayesian.additive.regression.tree(
#  all = all,
#  cutoff = .9,
#  num.tree = 100,
#  cutoff.coefficient = 1)