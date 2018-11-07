###################### CONVOLUTION NEURAL NETWORK ########################

# Define number of convolution layers and NN layers 
# in the function. 
# library('mxnet')

# Define function
cnn <- function(
  all = all,
  which.cpu = 1,
  # Convolution layers:
  num_filter_1 = 128,
  num_filter_2 = 64,
  num_filter_3 = 50,
  num_filter_4 = 50,
  num_filter_5 = 50,
  # NN layers:
  # Parameters:
  a1 = 128, #128+4*256 # LeCun: 128
  a2 = 64, #64+4*64 # LeCun: 64
  a3 = 10, #64 # LeCun: 10
  a4 = 10,
  a5 = 10,
  iteration = 50,
  # Cutoff: 
  data.cutoff.line = 0.9, #(36/42)
  cutoff.coefficient = 1
) {
  ################# SPLIT DATA ############################
  
  # Load train and test datasets
  train <- all[1:(data.cutoff.line*nrow(all)),]; dim(train)
  test <- all[(data.cutoff.line*nrow(all)+1):nrow(all),]; dim(test)
  
  # Set up train and test datasets
  train <- data.matrix(train)
  train_x <- t(train[, -1])
  train_y <- train[, 1]
  train_array <- train_x
  size <- round(sqrt(nrow(train_x)))
  dim(train_array) <- c(size, size, 1, ncol(train_x))
  
  test_x <- t(test[, -1])
  test_y <- test[, 1]
  test_array <- test_x
  dim(test_array) <- c(size, size, 1, ncol(test_x))
  
  #################### DESIGN CONVOLUTION LAYER #################
  
  # Set up the symbolic model
  data <- mx.symbol.Variable('data')
  
  # DESIGN CONVOLUTION LAYER:
  # 1st convolutional layer
  conv_1 <- mx.symbol.Convolution(data = data, kernel = c(5, 5), num_filter = num_filter_1) 
  tanh_1 <- mx.symbol.Activation(data = conv_1, act_type = "tanh")
  pool_1 <- mx.symbol.Pooling(data = tanh_1, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  # 2nd convolutional layer
  conv_2 <- mx.symbol.Convolution(data = pool_1, kernel = c(5, 5), num_filter = num_filter_2) # LeCun: 50
  tanh_2 <- mx.symbol.Activation(data = conv_2, act_type = "tanh")
  pool_2 <- mx.symbol.Pooling(data=tanh_2, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  # 3rd convolutional layer
  conv_3 <- mx.symbol.Convolution(data = pool_2, kernel = c(3, 3), num_filter = num_filter_3)
  tanh_3 <- mx.symbol.Activation(data = conv_3, act_type = "tanh")
  pool_3 <- mx.symbol.Pooling(data=tanh_3, pool_type = "max", kernel = c(2, 2), stride = c(2, 2))
  # 4th convolutional layer
  conv_4 <- mx.symbol.Convolution(data = pool_3, kernel = c(1, 1), num_filter = num_filter_4)
  tanh_4 <- mx.symbol.Activation(data = conv_4, act_type = "tanh")
  pool_4 <- mx.symbol.Pooling(data=tanh_4, pool_type = "max", kernel = c(1, 1), stride = c(1, 1))
  # 5th convolutional layer
  conv_5 <- mx.symbol.Convolution(data = pool_4, kernel = c(1, 1), num_filter = num_filter_5)
  tanh_5 <- mx.symbol.Activation(data = conv_5, act_type = "tanh")
  pool_5 <- mx.symbol.Pooling(data=tanh_5, pool_type = "max", kernel = c(1, 1), stride = c(1, 1))
  # 1st fully connected layer
  
  # CHOOSE HOW MANY CONV LAYERS?
  flatten <- mx.symbol.Flatten(data = pool_2) # Watch which pool_i to use
  fc_1 <- mx.symbol.FullyConnected(data = flatten, num_hidden = a1) # LeCun: 500 
  tanh_6 <- mx.symbol.Activation(data = fc_1, act_type = "tanh")
  # 2nd fully connected layer
  fc_2 <- mx.symbol.FullyConnected(data = tanh_6, num_hidden = a2) # LeCun: 40
  tanh_7 <- mx.symbol.Activation(data = fc_2, act_type = "tanh")
  # 3rd fully connected layer
  fc_3 <- mx.symbol.FullyConnected(data = tanh_7, num_hidden = a3)
  tanh_8 <- mx.symbol.Activation(data = fc_3, act_type = "tanh")
  # 4th fully connected layer
  fc_4 <- mx.symbol.FullyConnected(data = tanh_8, num_hidden = a4)
  tanh_9 <- mx.symbol.Activation(data = fc_4, act_type = "tanh")
  # 5th fully connected layer
  fc_5 <- mx.symbol.FullyConnected(data = tanh_9, num_hidden = a5)
  
  # CHOOSE HOW MANY NN LAYERS?
  # Output. Softmax output since we'd like to get some probabilities.
  NN_model <- mx.symbol.SoftmaxOutput(data = fc_3)
  
  #################### TRAINING ##################################
  
  # Pre-training set up: 
  # Set seed for reproducibility
  mx.set.seed(100)
  
  # Device used. CPU in my case.
  devices <- mx.cpu(which.cpu)
  # Device used. GPU in my case.
  # devices <- mx.gpu(which.cpu)

  # Training
  iter <- iteration
  
  # Train the model
  model <- mx.model.FeedForward.create(
    NN_model, 
    X = train_array,
    y = train_y,
    ctx = devices,
    num.round = iter, # LeCun: 480
    array.batch.size = 10,
    learning.rate = 0.01, # LeCun: 0.01
    momentum = 0.9, # LeCun: 0.9
    eval.metric = mx.metric.accuracy,
    epoch.end.callback = mx.callback.log.train.metric(100)
  )
  
  ###################### PREDICTION #########################
  
  preds <- predict(model, test_x)
  
  # It is a matrix  
  # containing the desired classification 
  # probabilities from the output layer. 
  # To extract the maximum label for each row, use max.col:
  
  #pred.label <- max.col(t(preds)) - 1
  pred.prob <- rowMeans(t(preds))
  pred.prob.mean <- mean(pred.prob)
  pred.label <- ifelse(pred.prob > cutoff.coefficient*pred.prob.mean, 1, 0)
  compare.truth.prob.table <- data.frame(test_y, pred.prob)
  compare.truth.lab.table <- data.frame(test_y, pred.label)
  table <- table(pred.label, test_y)
  percent <- sum(diag(table(pred.label, test_y)))/sum(table(pred.label, test_y))
  
  # ROC
  #actuals <- test_y
  #scores <- pred.label
  #library(pROC)
  #roc_obj <- roc(response = actuals, predictor =  scores)
  #auc <- auc(roc_obj)
  
  # Final results:
  return(
    list(
      Prediction.Table = table,
      Testing.Accuracy = percent,
      Testing.Error = 1-percent,
      #AUC = auc,
      #Gini.Co = 2*auc - 1,
      Truth.Prob.Table = compare.truth.prob.table,
      Truth.Label.Table = compare.truth.lab.table
    )
  )
} # End of function.

# Result
Result <- cnn(
  all = data.frame(rbind(train, test))[1:1500, ], 
  which.cpu = 1,
  # Convolution layers:
  num_filter_1 = 128,
  num_filter_2 = 64,
  num_filter_3 = 50,
  num_filter_4 = 50,
  num_filter_5 = 50,
  # NN layers:
  a1 = 128, 
  a2 = 64, 
  a3 = 10, 
  a4 = 10,
  a5 = 10,
  iteration = 5,
  # Cutoff: 
  data.cutoff.line = 0.9,
  cutoff.coefficient = 1
)
Result$Prediction.Table; Result$Testing.Accuracy