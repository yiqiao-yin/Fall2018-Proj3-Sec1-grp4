#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

# Input
#LR_dir <- train_LR_dir
#HR_dir <- train_HR_dir
#n_points = 1000

feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
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
  n_files <- 5
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  temp = 0
  
  # Define function to fill up featMat
  fill.up.feat <- function(color.i){
    # Randomly select a coordinate
    sample_n_points_row <- sample(2:(dim(imgLR)[1]-1), 1, replace = TRUE)
    sample_n_points_col <- sample(2:(dim(imgLR)[2]-1), 1, replace = TRUE)
    #set.seed(sample_n_points_col)
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
    return(c(feat.feat))
  }
  
  # Define function to fill up labMat
  fill.up.lab <- function(color.i){
    # Randomly select a coordinate
    sample_n_points_row <- sample(2:(dim(imgLR)[1]-1), 1, replace = TRUE)
    sample_n_points_col <- sample(2:(dim(imgLR)[2]-1), 1, replace = TRUE)
    #set.seed(sample_n_points_col)
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
    for (color.i in 1:3) {
      # Each color
      # color.i = 1
      feat.feat <- NA
      lab.lab <- NA
      
      # Define function to fill up featMat
      fill.up.feat.unit <- replicate(n_points, fill.up.feat(color.i))
      fill.up.feat.unit <- matrix(fill.up.feat.unit, ncol = 8)
      featMat[(temp+1):(i*n_points),,color.i] <- fill.up.feat.unit
      
      # Define function to fill up labMat
      fill.up.lab.unit <- rep(fill.up.lab(color.i), n_points)
      #fill.up.lab.unit <- lapply(n_points, fill.up.lab(color.i))
      fill.up.lab.unit <- matrix(fill.up.lab.unit, ncol = 4)
      labMat[(temp+1):(i*n_points),,color.i] <- fill.up.lab.unit
      
      # Finished with this color, e.g. this channel
      #print(paste0("Done with color ", color.i, " for picture ", i))
    }; # end.time <- Sys.time(); end.time - begin.time
    temp <- i*n_points
    print(paste0("Finished with picture No. ", i))
  }; end.time <- Sys.time(); end.time - begin.time
  
  # + `feature.R`
  # + Input: a path for low-resolution images.
  # + Input: a path for high-resolution images.
  # + Output: an RData file that contains extracted features and corresponding responses
  
  # Return
  return(list(
    feature = featMat, 
    label = labMat)
    )
}
