# This model was created for a homework assignment using the Credit Approval Data Set from the UCI Machine Learning Repository.
# The goal is to use a K-Nearest Neighbors model to classify applicants as either being approved or not.

#Function to install packages and load them if they are not already installed
PackagesPrep <- function(x){
  for( i in x ){
    #  require returns TRUE invisibly if it was able to load package
    if( ! require( i , character.only = TRUE ) ){
      #  If package was not able to be loaded then re-install
      install.packages( i , dependencies = TRUE )
      #  Load package after installing
      require( i , character.only = TRUE )
    }
  }
}

#  Run function
PackagesPrep( c("kknn" ) )

# Clear environment
rm(list = ls())

# Reading in the data
data <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)
data <- as.data.frame(data)


#
# check to make sure the data is read correctly
#

head(data)

# A1    A2    A3   A8 A9 A10 A11 A12 A14 A15 R1
# 1  1 30.83 0.000 1.25  1   0   1   1 202   0  1
# 2  0 58.67 4.460 3.04  1   0   6   1  43 560  1
# 3  0 24.50 0.500 1.50  1   1   0   1 280 824  1
# 4  1 27.83 1.540 3.75  1   0   5   0 100   3  1
# 5  1 20.17 5.625 1.71  1   1   0   1 120   0  1
# 6  1 32.08 4.000 2.50  1   1   0   0 360   0  1

# R1 is response, other variables are predictors

####################### Fitting the model#######################

# Setting the random number generator seed
set.seed(1)

# set maximum value of k (number of neighbors) to test
kmax <- 30

# create array of prediction qualities
accuracy_cv <- rep(0,kmax)

# calculate prediction qualities
for (k in 1:kmax) {
    # run cross-validation for each value of k (number of neighbors)
    model <- cv.kknn(R1~.,data,
                                        kcv=10, # 10-fold cross-validation
                                        k=k, # number of neighbors
                                        scale=TRUE) # scale data
    predicted <- as.integer(model[[1]][,2] + 0.5) # round off continuous results to 0 or 1
    accuracy_cv[k] <- sum(predicted == data$R1)/length(data$R1)
}
# show accuracies
accuracy_cv

# [1] 0.8012232 0.8149847 0.8165138 0.8042813 0.8394495 0.8562691 0.8440367 0.8440367 0.8516820 0.8516820 0.8470948 0.8501529 0.8501529 0.8532110 0.8318043 0.8440367 0.8623853 0.8425076 0.8516820
# [20] 0.8532110 0.8425076 0.8532110 0.8486239 0.8409786 0.8455657 0.8333333 0.8394495 0.8363914 0.8470948 0.8455657

#Print index of highest accuracy result along with accuracy
which(accuracy_cv == max(accuracy_cv))
  # [1] 17
max(accuracy_cv)
  # [1] 0.8623853

##Accuracy was used to select the optimal model using  the largest value.
##The final value used for the model was k = 17 with an accuracy of 86.2% success rate.