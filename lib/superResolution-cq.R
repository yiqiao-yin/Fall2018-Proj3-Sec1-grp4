########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3
#LR_dir <- test_LR_dir
#HR_dir <- test_HR_dir
#modelList <- fit_train

superResolution <- function( LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  #LR_dir <- paste0(path, "/data/test_set/LR/")
  
  n_files <- length(list.files(LR_dir))
  ### read LR/HR image pairs
  require(parallel)
  
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    #dim(imgLR)
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    n_row <- dim(imgLR)[1]
    n_col <- dim(imgLR)[2]
    imgLR <- abind(array(0, replace(dim(imgLR), 2, 1)),imgLR, array(0, replace(dim(imgLR), 2, 1)), along = 2)
    imgLR <- abind(array(0, replace(dim(imgLR), 1, 1)),imgLR, array(0, replace(dim(imgLR), 1, 1)), along = 1)

    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    #L <- min(dim(imgLR)[1], dim(imgLR)[2])
    #dim(imgLR)
    for (i in 1:n_row){ 
      for(j in 1:n_col){
      for (c in 1:3){
        neighbour8 <- c(imgLR[i,j:(j+2),c], imgLR[(i+1), j, c],imgLR[(i+1), (j+2), c], imgLR[(i+2), j:(j+2), c])  
        featMat[i*j, 1:8, c] <-neighbour8 - imgLR[i+1,j+1,c]
      }
    }
    }
  
    dim(featMat)
    ### step 2. apply the modelList over featMat
    predMat <- test(fit_train, featMat)
    length(predMat)
    predMat <- array(predMat, c(n_row * n_col, 4, 3)); 
    dim(predMat)
    ### step 3. recover high-resolution from predMat and save in HR_dir
    #imgHR <- array(0, c(sqrt(prod(dim(predMat))/3), sqrt(prod(dim(predMat))/3), 3));
    imgHR<-array(0,c(n_row*2,n_col*2,c))
    #dim(imgHR)
    imgHR[seq(1, nrow(imgHR), 2), seq(1, ncol(imgHR), 2), ] <- predMat[, 1, ]; 
    imgHR[seq(1, nrow(imgHR), 2), seq(2, ncol(imgHR), 2), ] <- predMat[, 2, ]; 
    imgHR[seq(2, nrow(imgHR), 2), seq(1, ncol(imgHR), 2), ] <- predMat[, 3, ]; 
    imgHR[seq(2, nrow(imgHR), 2), seq(2, ncol(imgHR), 2), ] <- predMat[, 4, ];
    #display(imgLR); display(imgHR)
    # Save
    library(jpeg)
    writeJPEG(imgHR, target = pathHR)
    #save(predMat, file=pathHR)
    }
  }
