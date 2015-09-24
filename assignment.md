Practical Machine Learning Assignment
=====================================

## Overview
Using the supplied data, located at the locataions below, a model was created to predict how well a person performed an excersise base off various data points.
Training data URL: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv]
Testing data URL: [https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv]

## Setup
The following libraries were used.  If you don't have them installed, you will need to do so before using them.
```{r}
library(caret)
library(rattle)
library(rpart)
library(rpart.plot)
```

##Data Processing
Load the data that was supplied.
```{r}
testing_raw<-read.csv("pml-testing.csv",header=TRUE,na.strings = "NA")
training_raw<-read.csv("pml-training.csv",header=TRUE,na.strings = "NA")
```

Set a seed so the results are reproduceable.  
```{r}
set.seed(584123)
```

The data will then be cleaned to remove any variables that are not numeric, mostly empty or are timestamps.
```{r}
nums<- sapply(training_raw,is.numeric) # find variables that are numeric
nums[2:7]=FALSE # remove timestamps and window
nums[160]=TRUE # keep last variable that contains the classe to predict
training<-training_raw[,nums] # update data with just the variables above
training<-training[,colSums(is.na(training)) < nrow(training)*0.5] # if columns are more than 50% NA's, drop it
```

Remove fields with high correlation.
```{r}
corm<-cor(training[,-54]) 
corval<-findCorrelation(corm)# find correlated variables
training<-training[,-corval] # remove these variables
```

The training data will now be split into a training set and a validation set.  This cross validation will be used to see how well the  model is able to predict on data that we know the answer to.
```{r}
inTrain <- createDataPartition(y=training$classe, p=0.6, list=FALSE)
training_set <-training[inTrain,]
validation_set <-training[-inTrain,]
```

A model is now created with the training set and then validation set is used to predict.  The results shown contain the error rate and accuracy.  A random forest model was chosen since there are 5 levels to predict from.  The error rate was < 1%.
```{r cache=TRUE}
modFit <- train(classe ~. , method="rf", data=training_set, prox=TRUE)
predictions<-predict(modFit,newdata = validation_set) # Use the model created to predict on the validation set
confusionMatrix(predictions,validation_set$classe) # Look at the results of the predictions with the correct response from validation set
```

Finally the test data will be used with this model to predict the final values.
```{r}
p<-predict(modFit,newdata = testing_raw)
p
```