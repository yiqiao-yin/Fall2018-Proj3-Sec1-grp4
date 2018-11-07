########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

superResolution <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    #imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d",i), ".jpg"))
    imgLR <- readImage("img_0001.jpg")
    
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
    
  }
}
plot(imgLR@.Data)
writeImage(imgMat,"test.jpg")



