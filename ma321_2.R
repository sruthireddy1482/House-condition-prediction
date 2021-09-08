library(dplyr)
library(ggplot2)
library(purrr)
library(caTools)
library(nnet)
library(mice)
library(caret)
library(data.table)
library(car)
library(MASS)
library(randomForest)

set.seed(123) #we need to set seed to achieve same results everytime
house_data_2=read.csv("https://raw.githubusercontent.com/sruthireddy1482/MA321_GRP18/main/house-data.csv",stringsAsFactors = T) #reading csv file
View(house_data_2)
dim(house_data_2)
#finding missing values in each column 
map(house_data_2, ~sum(is.na(.)))
str(house_data_2)
house_data_2 <- house_data_2[-c(945),]
#dividing house based on their overall condition
house_data_2$Overallcond <- ifelse(house_data_2$OverallCond>=1 & house_data_2$OverallCond <= 3, "Poor", "Good")
house_data_2$Overallcond <- ifelse(house_data_2$OverallCond >= 4 & house_data_2$OverallCond <= 6, "Average", house_data_2$Overallcond)
house_data_2$Overallcond <- ifelse(house_data_2$OverallCond >= 7 & house_data_2$OverallCond <= 10, "Good", house_data_2$Overallcond)

#viewing the overall condition values
table(house_data_2$Overallcond)
house_data_2$Overallcond=as.factor(house_data_2$Overallcond)
#removing columns with almost all missing values
data_2 <- subset(house_data_2, select = -c(Alley, Fence,PoolQC,MiscFeature))
View(data_2)
#imputing missing values of all the columns in te dataset
impute_2= mice(data_2[,c(2,18,22,23,37,39)])
print(impute_2)
#impute_2$imp$LotFrontage
new_data_2=complete(impute_2,1)
new_data_2

#replacing columns with mising values with imputed columns
data_2[,c(2,18,22,23,37,39)]=new_data_2[,c(1,2,3,4,5,6)]
sum(is.na(data_2))
summary(data_2)

`#Question 2(a)
#splitting the dataset into training and test sets
split_2a= sample.split(data_2$Overallcond, SplitRatio = 0.8)
training_set_2a=subset(data_2,split_2a==TRUE)
test_set_2a=subset(data_2,split_2a==FALSE)

#fitting multinomial logistic regression on training data
training_set_2a$Overallcond <- relevel(training_set_2a$Overallcond, ref = "Average")
multinom.fit_2a <- multinom(Overallcond~., data = training_set_2a)
`
summary(multinom.fit_2a)
#head(probability.table <- fitted(multinom.fit_2a))

#predicting on training data
pred_2a <- predict(multinom.fit_2a, newdata = training_set_2a, "class")
#pred_2a
confusionMatrix(pred_2a, training_set_2a$Overallcond)

#predicting on test data
pred2_2a= predict(multinom.fit_2a, newdata = test_set_2a, "class")
#pred2_2a
cm_2a=confusionMatrix(pred2_2a, test_set_2a$Overallcond)
cm_2a

#Question 2(b)
#Fitting Random forest model.
# sample.split() function is used to split data for training and test.
data_2b <- copy(data_2)
split_2b= sample.split(data_2b$Overallcond, SplitRatio = 0.8)
training_set_2b=subset(data_2b,split_2b == TRUE)
test_set_2b=subset(data_2b,split_2b == FALSE)

#Random forest on training data set
# by default ntree value is 500
rf_2b <- randomForest(Overallcond~., data=training_set_2b)

#To achieve more accurate results, we passed ntree vales =300 to check the oob estimate of error
#rf_2b <- randomForest(Overallcond~., data=training_set_2b,ntree=300)

print(rf_2b)
attributes(rf_2b)
prediction1_2b <- predict(rf_2b, training_set_2b)
#prediction1_2b
confusionMatrix(prediction1_2b, training_set_2b$Overallcond)
#Now lets predict test data
#Prediction & Confusion Matrix - test data
prediction2_2b <- predict(rf_2b, test_set_2b)
#prediction2_2b
confusionMatrix(prediction2_2b, test_set_2b$Overallcond)



