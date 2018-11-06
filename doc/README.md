# Project: Can you unscramble a blurry image? 

### Doc folder

* To run this script,just run our final report: the main.rmd file. (don't forget to link code here) 

**Below is a list of the inputs the main.rmd file takes in:**

1) feature.R : This script extracts the 8 neighboring pixels on a selected pixel from a low res image and uses it as a variable for training purposes. It is the wrapper for all our engineering functions and options. 
As inputs, it consists of paths for low-res and high-res images and as an output, it creates an array of feature extractions saved as a .Rdata file. 

2) crossvalidation.R : This script performs a cross validation on the training set of our image data 

3) train.R: contains the training steps for the model. As input, it takes in a path that points toe the training set features and responses and as an output, it creates trained classifiers in the form of R objects: models/settings/links to external trained configurations.

4) test.R: contains the steps for classification/prediction. As inputs, it takes in a path that points to the test set features, and an R object that contains a trained classifier.
As output, it produces an R object of response predictions of the test set. 

5) SuperResolution.R: This script takes in a path that points toe fhe folder of low-resolution test images, a path that pointsto an empty folder of high-resolution test images, an R object with tuned predictors and as an output, it constructs high-resolution versions of each low-resolution test image. 

* The main.rmd (link code here) consists of two parts: 

**Part 1**: Follows the structure and work flow of the baseline model 

**Part2** : Follows the structure and work flow of both our improved models: a linear regression and an xgboost model. 

* The output of our final report, main.rmd will be saved in two directories: 

1) The output folder (link here)

2) The SR-B folder located in the test set folder of the data folder 





The doc directory contains the report or presentation files. It can have subfolders.  
