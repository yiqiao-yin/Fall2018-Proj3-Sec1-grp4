########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3


# Extract eight neighbors for each pixcel for each RGB
eight_nei <- function(color.i, img){
  row <- dim(img)[1] 
  col <- dim(img)[2] 
  mat_base <- array(img,c(row, col, 3))
  
  mat_1 <- abind(array(rep(0, row-1), c(row-1,1,3)), array(img[-row,-col,], c(row-1, col-1, 3)), along=2)
  mat_1 <- abind(array(rep(0, col), c(1,col,3)), array(mat_1, c(row-1, col, 3)), along=1)
  mat_1_channel <- mat_1[,,color.i] - mat_base[,,color.i]
  
  mat_2 <- abind(array(rep(0, col), c(1,col,3)), array(img[-row,,], c(row-1, col, 3)), along=1)
  mat_2_channel <- mat_2[,,color.i] - mat_base[,,color.i]
  
  mat_3 <- abind(array(rep(0, col), c(1,col-1,3)), array(img[-row,-1,], c(row-1, col-1, 3)), along=1)
  mat_3 <- abind(array(mat_3, c(row, col-1, 3)), array(rep(0, row), c(row,1,3)), along=2)
  mat_3_channel <- mat_3[,,color.i] - mat_base[,,color.i]
  
  mat_4 <- abind(array(rep(0, row), c(row,1,3)), array(img[,-col,], c(row, col-1, 3)), along=2)
  mat_4_channel <- mat_4[,,color.i] - mat_base[,,color.i]
  mat_4_channel <- mat_4[,,color.i] - mat_base[,,color.i]
  
  mat_6 <- abind(array(img[,-1,], c(row, col-1, 3)),array(rep(0, row), c(row,1,3)), along=2)
  mat_6_channel <- mat_6[,,color.i] - mat_base[,,color.i]
  
  mat_7 <- abind(array(rep(0, row-1), c(row-1,1,3)), array(img[-1,-col,], c(row-1, col-1, 3)), along=2)
  mat_7 <- abind(array(mat_7, c(row-1, col, 3)), array(rep(0, col), c(1,col,3)), along=1)
  mat_7_channel <- mat_7[,,color.i] - mat_base[,,color.i]
  
  mat_8 <- abind(array(img[-1,,], c(row-1, col, 3)), array(rep(0, col), c(1,col,3)), along=1)
  mat_8_channel <- mat_8[,,color.i] - mat_base[,,color.i]
  
  mat_9 <- abind(array(img[-1,-1,], c(row-1, col-1, 3)), array(rep(0, row-1), c(row-1,1,3)), along=2)
  mat_9 <- abind(array(mat_9, c(row-1, col, 3)), array(rep(0, col), c(1,col,3)), along=1)
  mat_9_channel <- mat_9[,,color.i] - mat_base[,,color.i]
  
  mat_channel <- abind(mat_1_channel, mat_2_channel,mat_3_channel, 
                       mat_4_channel,mat_6_channel, mat_7_channel, 
                       mat_8_channel,mat_9_channel, along = 0)
  return(aperm(mat_channel,c(2,3,1)))
}


#LR_dir <- test_LR_dir
#HR_dir <- test_HR_dir
#modelList <- fit_train

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  LR_dir <- paste0(path, "/data/test_set/LR/")
  n_files <- length(list.files(LR_dir))
  #n_files <- 1
  
  ### read LR/HR image pairs
  for (i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    #imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d",i), ".jpg"))
    #imgLR <- readImage("img_0001.jpg")
    
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    for(j in 1:3){
      imgLR@.Data[1,,j] <- 0
      imgLR@.Data[dim(imgLR)[1],,j] <- 0
      imgLR@.Data[,1,j] <- 0
      imgLR@.Data[,dim(imgLR)[2],j] <- 0
    }
    
    featMat[,,1] <- array(as.numeric(eight_nei(color.i=1,imgLR)),dim = c(dim(imgLR)[1]*dim(imgLR)[2],8))
    featMat[,,2] <- array(as.numeric(eight_nei(color.i=2,imgLR)),dim = c(dim(imgLR)[1]*dim(imgLR)[2],8))
    featMat[,,3] <- array(as.numeric(eight_nei(color.i=3,imgLR)),dim = c(dim(imgLR)[1]*dim(imgLR)[2],8))
    
    
    ###           tips: padding zeros for boundary points
    
    ### step 2. apply the modelList over featMat
    #####predMat <- test(modelList, featMat)
    pred <- test(fit_train, featMat)
    predMat <- array(pred, dim = c(dim(imgLR)[1]*dim(imgLR)[2],4,3))
    #predM
    ### step 3. recover high-resolution from predMat and save in HR_dir
    imgMat <- array(NA, c(dim(imgLR)[1]*2,dim(imgLR)[2]*2, 3))
    imgMat[seq(1,dim(imgLR)[1]*2,2),seq(1,dim(imgLR)[2]*2,2),] <- predMat[,1,]
    imgMat[seq(1,dim(imgLR)[1]*2,2),seq(2,dim(imgLR)[2]*2,2),] <- predMat[,2,]
    imgMat[seq(2,dim(imgLR)[1]*2,2),seq(1,dim(imgLR)[2]*2,2),] <- predMat[,3,]
    imgMat[seq(2,dim(imgLR)[1]*2,2),seq(2,dim(imgLR)[2]*2,2),] <- predMat[,4,]
    imgHR <- predMat
    #display(imgLR); display(imgHR)
    
    # Save
    library(jpeg)
    writeJPEG(imgHR, target = pathHR)
    #save(predMat, file=pathHR)
  }
}