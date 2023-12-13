# Machine-Learning
ML Project
#Get working directory
```{r}
getwd()
library("readr")
```
# Training data and testing data downloaded
```{r}
training <-read.csv("pml-training.csv")        
testing <-read.csv("pml-testing.csv")
dim(training)
dim(testing)
```
# Subset data
```{r}
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]
```
#Removing Variables which are having NAvalues.Our threshold is 95%
```{r}
na.val.col <- sapply(training,function(x)mean(is.na(x))) > 0.95
training <- training[,na.val.col==FALSE]
testing <- testing[,na.val.col==FALSE]
dim(training)                 
dim(testing)
```
#Removing variables which are non-numeric and not contributing to our model
```{r}
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(lattice)
library(ggplot2)
library(gbm)
```
#Data cleaning
```{r}
training <- training[,-c(1:7)]
testing <- testing[,-c(1:7)]
colnames.training <- names(training)
colnames(training)
colnames(testing)                                  
library(randomForest)
rfNews()
na.val.col <- sapply(training, function(x) mean(is.na(x)))>0.95                           
set.seed(175)
SubcolumnIndex <- colSums(is.na(training))/nrow(training) <0.95
clean.training <- training[,SubcolumnIndex]
```
#We then verifying we have removed NA correctly
```{r}
colSums(is.na(clean.training))/nrow(clean.training)
colSums(is.na(clean.training))
```
# We remove column 1 to 7 because they are not related to the model
```{r}
clean.training <- clean.training[,-c(1:7)]
clean.testing <- testing[,-c(1:7)]
head(testing)
head(training)
library(gbm)
```
# Prepare Centre and Scale
```{r}
library(ggplot2)
library(caret)
non.zero.var <- nearZeroVar(training)
trains <- training[,-non.zero.var]
test <- testing[,-non.zero.var]
dim(training)
dim(testing)
```
#Removing varibles that are aving NA values.Our threshold is 95%
```{r}
na.val.col <- sapply(trains, function(x)mean(is.na(x)))> 0.95
trains <- trains[,na.val.col==FALSE]
test <- test[,na.val.col==FALSE]
dim(trains)
dim(test)
```
#Removing variables that are non numeric and will not contribute into the model
#Checking the training data has the "classe" variable in it and the testing has a
#"problem_id"variable in it.
```{r}
colnames.training <- names(trains)
```
#trains <- training[, c(colnames.training, "problem_id")]
```{r}
colnames(trains)
colnames(test)
```
# Data Partitioning
```{r}
intrainIndex <- createDataPartition(trains$classe,p= 0.6,list = FALSE)
intrain <- trains[intrainIndex,]
intest <- test[intrainIndex,]
dim(intrain)
dim(intest)
```
# Tree Decision model
```{r}
decisionTreeMod <- train(classe~.,data = trains,method ="rpart")
```
# Prediction in terms of Decision Tree Model
```{r}
rpart.plot(decisionTreeMod$finalModel,roundint = FALSE)
```
# Prediction
#Machine learning Algorithm- Random Forest
```{r}
rfMod <- train(classe~.,data = trains, method ="rf",ntree =100)
```
#Final Prediction 
```{r}
predict(rfMod,test)
```



