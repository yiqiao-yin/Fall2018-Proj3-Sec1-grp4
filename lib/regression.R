############## GLM: REGRESSION ######################

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
regression <- function(
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
  
  # Model Fitting
  model <- glm(
    train.y ~.,
    data=train.x,
    family = gaussian
  )
  summary(model)
  
  # Training result
  preds.train <- predict(model, train.x)
  preds.train <- data.frame(preds.train)
  mse.train <- mean((preds.train- train.y)^2)
  
  # Make prediction:
  preds <- predict(model, test.x)
  preds <- data.frame(preds)
  mse <- mean((preds - test.y)^2)
  
  # Final output:
  return(
    list(
      Training.MSE= mse.train,
      Testing.MSE = mse
    )
  )
} # End of function
