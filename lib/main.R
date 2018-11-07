############################### BEGIN SCRIPT ########################################
################################# MAIN ############################################

# Set the working directory
set.seed(2018)
path <- "C:/Users/eagle/Desktop/Fall2018-Proj3-Sec1-grp4-master"
setwd(path)

# Provide directories for training images.
train_dir <- paste0(path, "/data/train_set/") # This will be modified for different data sets.
train_LR_dir <- paste(train_dir, "LR/", sep="")
train_HR_dir <- paste(train_dir, "HR/", sep="")
train_label_path <- paste(train_dir, "label.csv", sep="") 

# In this chunk, we have a set of controls for the evaluation experiments. 

# + (T/F) cross-validation on the training set
# + (number) K, the number of CV folds
# + (T/F) process features for training set
# + (T/F) run evaluation on an independent test set
# + (T/F) process features for test set

run.cv=TRUE # run cross-validation on the training set
K <- 5  # number of CV folds
run.feature.train=TRUE # process features for training set
run.test=TRUE # run evaluation on an independent test set
run.feature.test=TRUE # process features for test set

# Using cross-validation
model_values <- seq(3, 11, 2)
model_labels = paste("GBM with depth =", model_values)

# We provide extra information
extra_label <- read.csv(train_label_path, colClasses=c("NULL", NA, NA))

########################## FEATURE EXTRACTION #############################

# Construct features and responses for training images 
# Input
LR_dir <- train_LR_dir
HR_dir <- train_HR_dir
n_points = 1000

### Construct process features for training images (LR/HR pairs)

### Input: a path for low-resolution images + a path for high-resolution images 
###        + number of points sampled from each LR image
### Output: an .RData file contains processed features and responses for the images

### load libraries
library("EBImage")
n_files <- length(list.files(LR_dir))
n_files = 2

### store feature and responses
featMat <- array(NA, c(n_files * n_points, 8, 3))
labMat <- array(NA, c(n_files * n_points, 4, 3))
temp = 0

### read LR/HR image pairs
begin.time <- Sys.time()
for(i in 1:n_files){
  # Each picture
  #i = 1
  imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
  imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
  
  # Instructions
  ### step 1. sample n_points from imgLR
  ### step 2. for each sampled point in imgLR,
  ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
  ###           tips: padding zeros for boundary points
  ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
  ### step 3. repeat above for three channels
  
  # According to the instructions above:
  # First, we update the matrix *featMat*
  # Second, we update the matrix *labMat*
  # begin.time <- Sys.time()
  
  # Each color
  #color.i = 1
  feat.feat <- NA
  lab.lab <- NA
  
  # Define function to fill up featMat
  fill.up.feat <- function(color.i){
    # Randomly select a coordinate
    sample_n_points_row <- sample(2:(dim(imgLR)[1]-1), 1, replace = FALSE)
    sample_n_points_col <- sample(2:(dim(imgLR)[2]-1), 1, replace = FALSE)
    # Update *featMat*
    center.point <- imgLR[sample_n_points_row, sample_n_points_col, color.i][1]
    feat.feat <- c(
      feat.feat,
      c(
        imgLR[sample_n_points_row-1, sample_n_points_col-1, color.i][1] - center.point,
        imgLR[sample_n_points_row-1, sample_n_points_col, color.i][1]- center.point,
        imgLR[sample_n_points_row-1, sample_n_points_col+1, color.i][1] - center.point,
        imgLR[sample_n_points_row, sample_n_points_col-1, color.i][1] - center.point,
        
        imgLR[sample_n_points_row, sample_n_points_col+1, color.i][1] - center.point,
        imgLR[sample_n_points_row+1, sample_n_points_col-1, color.i][1] - center.point,
        imgLR[sample_n_points_row+1, sample_n_points_col, color.i][1] - center.point,
        imgLR[sample_n_points_row+1, sample_n_points_col+1, color.i][1] - center.point
      ))[-1]
    c(feat.feat)
    return(c(feat.feat))
  }
  fill.up.feat.unit <- lapply(1:3, function(color.i){replicate(n_points, fill.up.feat(color.i))})
  fill.up.feat.unit <- lapply(1:3, function(color.i){t(matrix(c(fill.up.feat.unit[[color.i]]), nrow = 8))})
  fill.up.feat.unit <- array(unlist(fill.up.feat.unit), c(n_points, 8, 3))
  featMat[(temp+1):(i*n_points),,] <- fill.up.feat.unit
  
  # Define function to fill up labMat
  fill.up.lab <- function(color.i){
    # Randomly select a coordinate
    sample_n_points_row <- sample(2:(dim(imgLR)[1]-1), 1, replace = FALSE)
    sample_n_points_col <- sample(2:(dim(imgLR)[2]-1), 1, replace = FALSE)
    # Update *labMat*
    corresponding.row <- round(sample_n_points_row/nrow(imgLR)*nrow(imgHR))
    corresponding.col <- round(sample_n_points_col/ncol(imgLR)*ncol(imgHR))
    corresponding.center.point <- imgHR[corresponding.row, corresponding.col, color.i][1]
    lab.lab <- c(
      lab.lab,
      c(
        imgHR[2*sample_n_points_row, 2*sample_n_points_col, color.i][1],
        imgHR[2*sample_n_points_row, 2*sample_n_points_col+1, color.i][1],
        imgHR[2*sample_n_points_row+1, 2*sample_n_points_col, color.i][1],
        imgHR[2*sample_n_points_row+1, 2*sample_n_points_col+1, color.i][1]
      ))[-1]
    return(c(
      lab.lab
    ))
  }
  fill.up.lab.unit <- lapply(1:3, function(color.i){replicate(n_points, fill.up.lab(color.i))})
  fill.up.lab.unit <- lapply(1:3, function(color.i){t(matrix(c(fill.up.lab.unit[[color.i]]), nrow = 8))})
  fill.up.lab.unit <- array(unlist(fill.up.lab.unit), c(n_points, 4, 3))
  labMat[(temp+1):(i*n_points),,] <- fill.up.lab.unit
  
  # Store n_points
  temp <- i*n_points
  print(paste0("End with picture ", i))
}; end.time <- Sys.time(); end.time - begin.time

# + `feature.R`
# + Input: a path for low-resolution images.
# + Input: a path for high-resolution images.
# + Output: an RData file that contains extracted features and corresponding responses
dat_train <- list(
  feature = featMat, 
  label = labMat)

# Back up with new names
feature = featMat
label = labMat

# Back upwith new names
feat_train <- dat_train$feature
label_train <- dat_train$label

# Save
save(dat_train, file=paste0(path, "/output/feature_train_update.RData"))

########################## GBM: GRADIENT BOOST MACHINE #############################

# READ:
# This script calls the regression.R file
# which allows us to run the function regression()
# We will record the results (training error and validating error)
# in a list called *result*

# NOTE
# This script separate train/test relying on 
# regression() function which uses the cutoff
# parameter to cut the data set into train and test.
# For ex, a cutoff of 0.9 means that the first 90% of observations
# are used as training and the last 10% of observations 
# are used as validating.

# This is more efficient because train and test are 
# both in one function.

#source(paste0(path, "/lib/regression.R"))
source(paste0(path, "/lib/gbm.R"))
result <- list()
### this part can be parallelized
for (i in 1:12){
  ## calculate column and channel
  c1 <- (i-1) %% 4 + 1
  c2 <- (i-c1) %/% 4 + 1
  featMat <- feature[, , c2]
  labMat <- label[, c1, c2]
  all <- data.frame(cbind(labMat, featMat))
  # This is where we would add code to run cross validation
  # Please see the following section for updated code
  fit <- grad.boost.machine(
    all = all,
    cutoff = 0.9
  )
  # Store result
  result[i] <- list(fit)
  
  # Let us clean up result
  temp <- data.frame(matrix(unlist(result), nrow=2))
  rownames(temp) <- c("Train MSE", "Test MSE")
}

# Change column names
for (i in 1:12) {
  colnames(temp)[i] <- paste0("Machine ", i)
}

# Output
temp

# Save
save(temp, file=paste0(path, "/output/base_line_output.RData"))

# Average Test MSE
sum(temp[2,])/12

################################ CROSS VALIDATION ###################################

# Set parameter
# How many folds? e.g. What is K in K-fold cv?
#source(paste0(path, "/lib/regression.R"))
source(paste0(path, "/lib/gbm.R"))
K = 5
Final.Result <- list()
modelList <- list()
### this part can be parallelized
for (i in 1:12){
  ## calculate column and channel
  c1 <- (i-1) %% 4 + 1
  c2 <- (i-c1) %/% 4 + 1
  featMat <- feature[, , c2]
  labMat <- label[, c1, c2]
  all <- data.frame(cbind(labMat, featMat))
  
  # Here we conduct K-fold cross validation
  # Null Result:
  result.train <- NULL
  list.of.table <- NULL
  result <- NULL
  acc <- NULL
  
  # CV:
  # Write a k-fold CV loop:
  how.many.folds = K
  for (folds.i in 1:how.many.folds){
    # Create k-fold training data sets for CV:
    
    # Create:
    # folds: a list of numbers with different index;
    # testIndexes: the index that equals to each index in folds;
    folds <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
    
    # For regression
    # Set:
    all <- data.frame(rbind(
      all[folds != folds.i, ],
      all[folds == folds.i, ]
    ))
    
    # MODEL FITTING / MACHINE LEARNING:
    # One can change to use Regression:
    #round(nrow(all[folds != folds.i, ])/nrow(all), 1) # Check this ratio out
    Model.Result = grad.boost.machine(
      all = all,
      cutoff = round(nrow(all[folds != folds.i, ])/nrow(all), 1) # This will be train/test split ratio
    )
    modelList[i][folds.i] <- Model.Result
    
    # Print result
    result.train <- c(result.train, paste("Fold", folds.i), Model.Result$Training.MSE)
    #list.of.table <- list(list.of.table, Model.Result$Prediction.Table) # Classification only
    result <- c(result, paste("Fold", folds.i), Model.Result$Testing.MSE)
    acc <- mean(c(acc, Model.Result$Testing.MSE))
    print(paste("Done with fold", folds.i))
  }
  
  # Result
  print(list(
    Train.Result = t(matrix(result.train, nrow = 2)),
    Train.Result.Average = mean(as.numeric(t(matrix(result.train, nrow = 2))[,2])),
    Validate.Result = t(matrix(result, nrow = 2)),
    Ave.K.fold.CV.Acc = acc
  )) # End of cross validation
  print(paste0(
    ">>>>>>>>>>>>>>>>>>>This finishes machine ", i, " for all ", K, "-fold cross validation.<<<<<<<<<<<<<<<<<<<"
  ))
  
  # Store result
  Validate.Result = t(matrix(result, nrow = 2))
  colnames(Validate.Result) <- c("Fold", "Validate MSE")
  Final.Result[i] <- list(Validate.Result = Validate.Result)
}

# Output
Final.Result <- data.frame(matrix(unlist(Final.Result), nrow = 5))
Final.Result <- Final.Result[, -c(3,5,7,9,11,13,15,17,19,21,23)]
Final.Result

# Change column names
for (i in 2:13) {
  colnames(Final.Result)[i] <- paste0("Machine ", i-1)
}

# Output
Final.Result

# Save
save(Final.Result, file=paste0(path, "/output/base_line_k_fold_output.RData"))

# Average Test MSE
# SD Test MSE for All 12 AI Learners
cv.error <- NA
for (i in 1:12) {
  cv.error <- c(
    cv.error,
    mean(as.numeric(as.character(Final.Result[, i+1])))
  )
}
cv.error <- cv.error[-1]
mean(cv.error); sd(cv.error)

################################ TEST: HIGH RESOLUTION ###############################

### load libraries
library("EBImage")
LR_dir <- paste0(path, "/data/test_set/LR/")
n_files <- length(list.files(LR_dir))

### read LR/HR image pairs
for(i in 1:n_files){
  imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
  pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
  featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
  dim(featMat)
  feat.feat <- NA
  begin.row <- 0
  
  # Padding zeros for boundary
  imgLR[1,,] <- 0 # Padding zeros for boundary
  imgLR[,1,] <- 0 # Padding zeros for boundary
  imgLR[nrow(imgLR),,] <- 0 # Padding zeros for boundary
  imgLR[,ncol(imgLR),] <- 0 # Padding zeros for boundary
  
  ### step 1. for each pixel and each channel in imgLR:
  ###           save (the neighbor 8 pixels - central pixel) in featMat
  ###           tips: padding zeros for boundary points
  #L <- min(dim(imgLR)[1], dim(imgLR)[2])
  L <- 6
  fill.up.feat.unit <- lapply(1:3, 
                              function(color.i){
                                feat.feat <- NA
                                temp <- mapply(function(row.unit, col.unit){
                                  center.point <- imgLR[row.unit, col.unit, color.i][1]
                                  feat.feat <- c(
                                    feat.feat,
                                    c(
                                      imgLR[row.unit-1, col.unit-1, color.i][1] - center.point,
                                      imgLR[row.unit-1, col.unit, color.i][1]- center.point,
                                      imgLR[row.unit-1, col.unit+1, color.i][1] - center.point,
                                      imgLR[row.unit, col.unit-1, color.i][1] - center.point,
                                      
                                      imgLR[row.unit, col.unit+1, color.i][1] - center.point,
                                      imgLR[row.unit+1, col.unit-1, color.i][1] - center.point,
                                      imgLR[row.unit+1, col.unit, color.i][1] - center.point,
                                      imgLR[row.unit+1, col.unit+1, color.i][1] - center.point
                                    ))[-1]
                                },rep(2:(L-1),length(2:(L-1))),sort(rep(2:(L-1),length(2:(L-1)))))
                                temp <- t(temp)
                              })
  fill.up.feat.unit <- array(
    unlist(fill.up.feat.unit),
    c(length(rep(2:(L-1),length(2:(L-1)))), 8, 3)
  )
  featMat[(begin.row+1):(i*(length(rep(2:(L-1),length(2:(L-1)))))),,] <- fill.up.feat.unit
  
  ### step 2. apply the modelList over featMat
  predMat <- test(modelList, featMat)
  ### step 3. recover high-resolution from predMat and save in HR_dir
}

########################## BART: BAYESIAN ADDITIVE REGRESSION TREE #############################

# READ:
# This script calls the bart.R file
# which allows us to run the function bart()
# We will record the results (training error and validating error)
# in a list called *result*

# NOTE
# This script separate train/test relying on 
# bart() function which uses the cutoff
# parameter to cut the data set into train and test.
# For ex, a cutoff of 0.9 means that the first 90% of observations
# are used as training and the last 10% of observations 
# are used as validating.

# This is more efficient because train and test are 
# both in one function.

#source(paste0(path, "/lib/regression.R"))
source(paste0(path, "/lib/bart.R"))
result <- list()
### this part can be parallelized
for (i in 1:12){
  ## calculate column and channel
  c1 <- (i-1) %% 4 + 1
  c2 <- (i-c1) %/% 4 + 1
  featMat <- feature[, , c2]
  labMat <- label[, c1, c2]
  all <- data.frame(cbind(labMat, featMat))
  # This is where we would add code to run cross validation
  # Please see the following section for updated code
  fit <- bayesian.additive.regression.tree(
    all = all,
    cutoff = .9,
    num.tree = 200)
  # Store result
  result[i] <- list(fit)
  
  # Let us clean up result
  temp <- data.frame(matrix(unlist(result), nrow=2))
  rownames(temp) <- c("Train MSE", "Test MSE")
}

# Change column names
for (i in 1:12) {
  colnames(temp)[i] <- paste0("Machine ", i)
}

# Output
temp.result <- temp # Backup
temp.result

# Save
save(temp.result, file=paste0(path, "/output/improved_by_bart_output.RData"))

# Average Test MSE
sum(temp.result[2,])/12

################################ CROSS VALIDATION ###################################

# Set parameter
# How many folds? e.g. What is K in K-fold cv?
#source(paste0(path, "/lib/regression.R"))
source(paste0(path, "/lib/bart.R"))
K = 5
Final.Result <- list()
### this part can be parallelized
for (i in 1:12){
  ## calculate column and channel
  c1 <- (i-1) %% 4 + 1
  c2 <- (i-c1) %/% 4 + 1
  featMat <- feature[, , c2]
  labMat <- label[, c1, c2]
  all <- data.frame(cbind(labMat, featMat))
  
  # Here we conduct K-fold cross validation
  # Null Result:
  result.train <- NULL
  list.of.table <- NULL
  result <- NULL
  acc <- NULL
  
  # CV:
  # Write a k-fold CV loop:
  how.many.folds = K
  for (folds.i in 1:how.many.folds){
    # Create k-fold training data sets for CV:
    
    # Create:
    # folds: a list of numbers with different index;
    # testIndexes: the index that equals to each index in folds;
    folds <- cut(seq(1,nrow(all)),breaks=how.many.folds,labels=FALSE)
    
    # For regression
    # Set:
    all <- data.frame(rbind(
      all[folds != folds.i, ],
      all[folds == folds.i, ]
    ))
    
    # MODEL FITTING / MACHINE LEARNING:
    # One can change to use other regression problem, e.g. here we use BART:
    #round(nrow(all[folds != folds.i, ])/nrow(all), 1) # Check this ratio out
    Model.Result = bayesian.additive.regression.tree(
      all = all,
      cutoff = round(nrow(all[folds != folds.i, ])/nrow(all), 1),
      num.tree = 300)
    
    # Print result
    result.train <- c(result.train, paste("Fold", folds.i), Model.Result$Training.MSE)
    result <- c(result, paste("Fold", folds.i), Model.Result$Testing.MSE)
    acc <- mean(c(acc, Model.Result$Testing.MSE))
    print(paste("Done with fold", folds.i))
  }
  
  # Result
  print(list(
    Train.Result = t(matrix(result.train, nrow = 2)),
    Train.Result.Average = mean(as.numeric(t(matrix(result.train, nrow = 2))[,2])),
    Validate.Result = t(matrix(result, nrow = 2)),
    Ave.K.fold.CV.Acc = acc
  )) # End of cross validation
  print(paste0(
    ">>>>>>>>>>>>>>>>>>>This finishes machine ", i, " for all ", K, "-fold cross validation.<<<<<<<<<<<<<<<<<<<"
  ))
  
  # Store result
  Validate.Result = t(matrix(result, nrow = 2))
  colnames(Validate.Result) <- c("Fold", "Validate MSE")
  Final.Result[i] <- list(Validate.Result = Validate.Result)
}

# Output
Final.Result <- data.frame(matrix(unlist(Final.Result), nrow = 5))
Final.Result <- Final.Result[, -c(3,5,7,9,11,13,15,17,19,21,23)]
Final.Result

# Change column names
for (i in 2:13) {
  colnames(Final.Result)[i] <- paste0("Machine ", i-1)
}

# Output
Final.Result

# Save
save(Final.Result, file=paste0(path, "/output/improved_by_bart_k_fold_output.RData"))

# Average Test MSE
# SD Test MSE for All 12 AI Learners
cv.error <- NA
for (i in 1:12) {
  cv.error <- c(
    cv.error,
    mean(as.numeric(as.character(Final.Result[, i+1])))
  )
}
cv.error <- cv.error[-1]
mean(cv.error); sd(cv.error)

############################### END SCRIPT ########################################