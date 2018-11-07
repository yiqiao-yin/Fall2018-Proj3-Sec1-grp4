########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3
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
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg")); imgLR_backup <- imgLR
    L = 50 # Try resize to a smaller size to check whether this script is correct
    imgLR_small <- resize(imgLR, L, L); imgLR <- imgLR_small
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), "-draft", ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3)); dim(featMat)
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
                                  },
                                  rep(2:(L-1),length(2:(L-1))),
                                  sort(rep(2:(L-1),length(2:(L-1))))
                                  )
                                  temp <- t(temp)
                                })
    fill.up.feat.unit <- array(
      unlist(fill.up.feat.unit),
      c(length(rep(2:(L-1),length(2:(L-1)))), 8, 3)
    )
    featMat[(begin.row+1):((length(rep(2:(L-1),length(2:(L-1)))))),,] <- fill.up.feat.unit
    
    ### step 2. apply the modelList over featMat
    predMat <- train(modelList, featMat)
    predMat <- array(predMat, c(dim(imgLR)[1]*2 * dim(imgLR)[2]*2, 4, 3)); dim(predMat)
    
    ### step 3. recover high-resolution from predMat and save in HR_dir
    imgHR <- array(0, c(sqrt(prod(dim(predMat))/3), sqrt(prod(dim(predMat))/3), 3)); dim(imgHR)
    imgHR[seq(1, nrow(imgHR), 2), seq(1, nrow(imgHR), 2), ] <- predMat[, 1, ]; 
    imgHR[seq(2, nrow(imgHR), 2), seq(2, nrow(imgHR), 2), ] <- predMat[, 2, ]; 
    imgHR[seq(1, nrow(imgHR), 2), seq(2, nrow(imgHR), 2), ] <- predMat[, 3, ]; 
    imgHR[seq(2, nrow(imgHR), 2), seq(1, nrow(imgHR), 2), ] <- predMat[, 4, ];
    #display(imgLR); display(imgHR)

    # Save
    library(jpeg)
    writeJPEG(imgHR, target = pathHR)
    #save(predMat, file=pathHR)
  }
}