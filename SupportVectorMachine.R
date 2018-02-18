# This model was created for a homework assignment using the Credit Approval Data Set from the UCI Machine Learning Repository.
# The goal is to use a Support Vector Machine model to classify applicants as either being approved or not.

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
PackagesPrep( c("kernlab" ) )



# Clear environment
rm(list = ls())


# Reading in the data
data <- read.table("https://d37djvu3ytnwxt.cloudfront.net/assets/courseware/v1/39b78ff5c5c28981f009b54831d81649/asset-v1:GTx+ISYE6501x+2T2017+type@asset+block/credit_card_data-headers.txt", header = TRUE)
data <- as.data.frame(data)

#
# check to make sure the data is read correctly
#

head(data)

# Console output for head(data)
# A1    A2    A3   A8 A9 A10 A11 A12 A14 A15 R1
# 1  1 30.83 0.000 1.25  1   0   1   1 202   0  1
# 2  0 58.67 4.460 3.04  1   0   6   1  43 560  1
# 3  0 24.50 0.500 1.50  1   1   0   1 280 824  1
# 4  1 27.83 1.540 3.75  1   0   5   0 100   3  1
# 5  1 20.17 5.625 1.71  1   1   0   1 120   0  1
# 6  1 32.08 4.000 2.50  1   1   0   0 360   0  1

# R1 is response, other variables are predictors

# Setting the random number generator seed
set.seed(1)



####################### Fitting the model #######################

model_scaled <- ksvm(as.matrix(data[,1:10]),as.factor(data[,11]),
		   type = "C-svc", # Use C-classification method
              kernel = "vanilladot", # Use simple linear kernel
              C = 100,
		   scaled=TRUE) # scale data
              
model_scaled

# Console output for model_scaled
##
## Support Vector Machine object of class "ksvm"
## SV type: C-svc (classification)
## parameter : cost C = 100
## Linear (vanilla) kernel function.
## Number of Support Vectors : 189 
## Objective Function Value : -17887.92 
## Training error : 0.136086


####################### Calculating the a coefficients to determine our equation #######################
#
#Classification is done using linear kernel, a*scaled(x) + a0. 
# Unfortunately, the model does not output a directly, but we can use the model output to find a.
# calculate a1 to am using the stored data point values in the model data structure and corresponding coefficients
# multiplying the xmatrix by the coef output gives the linear combination of data points that define a1,...,am
# we use the xmatrix attribute since the model stores these data points as scaled

#adjust coefficients for scale changes
a_scaled <- colSums(model_scaled@xmatrix[[1]] * model_scaled@coef[[1]])

# a0 is just -model_scaled@b
a0_scaled<- -model_scaled@b

a_scaled
a0_scaled

#Console output for a_scaled 
# A1            A2            A3            A8            A9           A10           A11           A12           A14           A15 
# -0.0010065348 -0.0011729048 -0.0016261967  0.0030064203  1.0049405641 -0.0028259432  0.0002600295 -0.0005349551 -0.0012283758  0.1063633995 

#Console output for a0_scaled
## [1] 0.08158492

####################### Calculating the predicted values ####################### 
# There are two ways we can go about calculating this: using our a values and the predict function
# Let's do both methods - first I'll use the a values and then use the predict function for a comparison


#Create predicted vector (to hold our calculated predicted values)
predicted_scaled<-rep(0,nrow(data))

#For each data point, perform the transformation, calculate a*scaled(data point)+a0, 
#and predict value of data point based on the resulting value
for (i in 1:nrow(data)){

  #If the data point is above the classifier, predicted value = 1

  if (sum(a_scaled*(data[i,1:10]-model_scaled@scaling$x.scale$`scaled:center`)/model_scaled@scaling$x.scale$`scaled:scale`) + a0_scaled >= 0){
    predicted_scaled[i] <- 1
  }

  #If the data point is below the classifier, predicted value = 0

  if (sum(a_scaled*(data[i,1:10]-model_scaled@scaling$x.scale$`scaled:center`)/model_scaled@scaling$x.scale$`scaled:scale`) + a0_scaled < 0){
    predicted_scaled[i] <- 0
  }
}

predicted_scaled

# Output from predicted_scaled
##  [1] 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [42] 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1
##  [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [124] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [165] 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [206] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
##  [247] 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [288] 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [329] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [370] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [411] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [452] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [493] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
##  [534] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1
##  [575] 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
##  [616] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0


# Note that we could also get the predicted values of the model using model_scaled@fitted
#

pred_scaled <- predict(model_scaled,data[,1:10])
pred_scaled

# Output from pred_scaled
## [1] 1 1 1 1 1 1 1 1 1 1 0 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [42] 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1
## [83] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [124] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [165] 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [206] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0
## [247] 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [288] 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 1 0 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [329] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [370] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [411] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [452] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [493] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
## [534] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 1 1 1 1 0 1 1 1 1 1 1 0 0 1
## [575] 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
## [616] 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0

# It appears that the results are the same meaning the transformation was likely correct. Let's check them for accuracy now.

####################### Calculating the model's accuracy ####################### 

#Percent correct
sum(pred_scaled == data$V11) / nrow(data)
sum(predicted_scaled == data$V11) / nrow(data)

#Output from sum(pred_scaled == data$V11) / nrow(data)
## [1] 0.8639144
#
#Output from sum(predicted_scaled == data$V11) / nrow(data)
## [1] 0.8639144

#The results are the same!

# Note that I did a large amount of trial and error using different C values (the value that trades off how small the margin is versus smaller error
# and the range that achieves an accuracy of 86.39% is ~.002 to ~500, so a wide range of C values work in this scenario.

#classifier_equation: -0.001*X1 - 0.0012*X2 - 0.0016*X3 + 0.0030*X4 + 1.0049*X5 - 0.0028*X6 + 0.0003*X7 - 0.0005*X8 - 0.0012*X9 + 0.1064*X10 + 0.0816 = 0
#This equation results in 86.39% accuracy

