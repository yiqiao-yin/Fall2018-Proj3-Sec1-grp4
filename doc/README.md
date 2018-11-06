# Project: Can you unscramble a blurry image? 

### Doc folder

* To run this script,just run our final report: the main.rmd file. (don't forget to link code here) 

**Below is a list of the inputs the main.rmd file takes in:**

1) feature.R : This script extracts the 8 neighboring pixels on a selected pixel from a low res image and uses it as a variable for training purposes. It is the wrapper for all our engineering functions and options. 
As inputs, it consists of paths for low-res and high-res images and as an output, it creates an array of feature extractions saved as a .Rdata file. 

2) crossvalidation.R : This script performs a cross validation on the training set of our image data 

3) train.R: 'train.R' and 'test.R' contain the training steps and the classification/predictions steps for our models. As inputs, they take in a path that points toe the training set features and responses. 
As 


2) input of rmd file is in the lib folder
inputs are: 
feature.R, this script will extract 8 pixels on a selected pixel and use that as the varaible for training
crossvalidation.R
train.R
test.R
superresolution.R

3) this main.rmd has part 1 and part 2
part 1 will follow structure of baseline model

part 2 will follow structure of improved model

linear regression
xgboost

4) The output of main.rmd file is saved in two directories: 
output folder

data folder --> test set --> SR-B folder



The doc directory contains the report or presentation files. It can have subfolders.  
