# Packages setup
# Required to have the packages installed before using them
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
library(randomForest)

# Load test and training CSV data
# Training data URL: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# Testing data URL: https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

testing_raw<-read.csv("pml-testing.csv",header=TRUE,na.strings = "NA")
training_raw<-read.csv("pml-training.csv",header=TRUE,na.strings = "NA")

# Pick a seed so the results are reproduceable
set.seed(3)



## identify variables that contribute to predicting classe
nums<- sapply(training_raw,is.numeric) # find variables that are numeric
nums[1:7]=FALSE # remove timestamps and window
nums[160]=TRUE # keep last variable that contains the classe to predict
training<-training_raw[,nums] # update data with just the variables above
training<-training[,colSums(is.na(training)) < nrow(training)*0.5] # if columns are more than 50% NA's, drop it

corm<-cor(training[,-53]) 
corval<-findCorrelation(corm)# find correlated variables
training<-training[,-corval] # remove these variables

#no non zero vectors found
#nzv<-nearZeroVar(training,saveMetrics = TRUE) # look for variables that have high frequency


# Split the training data into a training and validation set.
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
training_set <-training[inTrain,]
validation_set <-training[-inTrain,]

# Create the model on the training set
modFit <- train(classe ~. , method="rf", data=training_set, prox=TRUE)
predictions<-predict(modFit,newdata = validation_set) # Use the model created to predict on the validation set
confusionMatrix(predictions,validation_set$classe) # Look at the results of the predictions with the correct response from validation set


# Predict variable 'classe' with the test data supplied
predict(modFit,newdata = testing_raw)


## cross validation is used with the validation set which was split from the training data.




### Creating predictions using PCA
preProc <- preProcess(training_set[,-53], method="pca") # preProcess the training set with PCA
trainPC <- predict(preProc, training_set[,-53]) # new PCA results are created by using the predict function with the same training set
modFit_PCA <- train(training_set$classe ~ . ,method="rf", data=trainPC) # train a model with the new PCA results

## testing prediction on validation set we know the answers to
testPC<-predict(preProc,validation_set[-53]) # the validation set will need to be converted with the same PCA results as the training set
confusionMatrix(validation_set$classe,predict(modFit_PCA,testPC)) # Look at the results to see how close the model matched the correct value

## Final prediction on test set proviced
testPC_final<-predict(preProc,testing[-53])
predict(modFit_PCA,testPC_final)
