#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

# Define function
eight_nei <- function(x,y,img){
  nei <- array(NA, c(length(x)*3,3,3))
  nei_v <- array(NA, c(length(x)*8,3))
  for (i in 1:length(x)) {
    if(x[i] == 1|y[i] == 1|x[i] == nrow(img)|y[i] == ncol(img)){
      nei[(3*i-2):(3*i),,] <- 0
    }
    else{
      row <- (x[i]-1):(x[i]+1)
      col <- (y[i]-1):(y[i]+1)
      nei[(3*i-2):(3*i),,] <- img[row,col,]
    }
    nei_v[(8*i-7):(8*i),] <- matrix(nei[(3*i-2):(3*i),,],(3*3),3)[-5,]
  }
  nei_a <- array(unlist(nei_v),dim=c(8,length(x),3))
  return(aperm(nei_a, c(2,1,3)))
}

# Define function
four_nei <- function(x,y,img){
  nei_v <- array(NA,c(length(x)*4,3))
  nei <- array(NA, c(length(x)*2,2,3))
  for (i in 1:length(x)) {
    row <- (2*x[i]-1):(2*x[i])
    col <- (2*y[i]-1):(2*y[i])
    nei[(2*i-1):(2*i),,] <- img[row,col,]
    nei_v[(4*i-3):(4*i),] <- matrix(nei[(2*i-1):(2*i),,], 2*2, 3)
  }
  nei_a <- array(unlist(nei_v),dim=c(4,length(x),3))
  return(aperm(nei_a, c(2,1,3)))
}

# Input
#LR_dir = train_LR_dir
#HR_dir = train_HR_dir

# Define function
feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  n_files <- 10
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
 
  ### read LR/HR image pairs
  ### create cluster object
  require(parallel)
  cl <- makeCluster(12)
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    ### step 1. sample n_points from imgLR
    sam_x <- sample(1:nrow(imgLR), n_points, replace = T)
    sam_y <- sample(1:ncol(imgLR), n_points, replace = T)
    ### step 2. for each sampled point in imgLR,
    featMat[(i*n_points-n_points+1):(i*n_points),,] <- eight_nei(sam_x, sam_y, imgLR)
    labMat[(i*n_points-n_points+1):(i*n_points),,] <- four_nei(sam_x, sam_y, imgHR)
    ### step 3. repeat above for three channels
  }
  ### close
  stopCluster(cl)
  return(list(feature = featMat, label = labMat))
}