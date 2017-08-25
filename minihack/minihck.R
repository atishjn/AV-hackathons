
library(xgboost)
library(randomForest)
install.packages("Cubist")
install.packages("forecast")

library(Cubist)
library(forecast)
library(stringr)
train <- read.csv(file.choose(),stringsAsFactors = FALSE)
test <- read.csv(file.choose(),stringsAsFactors = FALSE)
train_mini<- train
test_mini<- test

train$hour <- as.numeric(str_sub(train$Datetime,12,13))
train$month <- as.numeric(str_sub(train$Datetime,4,5))
train$year <- as.numeric(str_sub(train$Datetime,7,10))
test$hour <- as.numeric(str_sub(test$Datetime,12,13))
test$month <- as.numeric(str_sub(test$Datetime,4,5))
test$year <- as.numeric(str_sub(test$Datetime,7,10))
head(train)
class(test$Datetime)
str(train)

model <- glm(Count~hour+month+year,data=train[17001:18244,])
model4 <- lm(Count~hour+month+year,data=train[17001:18244,])
summary(model4)

model2<- randomForest(Count~hour+month+year,data=train[17001:18244,])
model2<- randomForest(Count~hour+month+year,data=train)

nrow(train)

pred <- predict(fitg,test[,c("hour","month","year")])
prediction <- data.frame(Datetime=test$Datetime,Count=pred)
write.csv(prediction,file="prediction11.csv",row.names=FALSE)

library(gbm)
fitg <- gbm(Count~hour+month+year,data=train[17001:18244,],n.trees=10,shrinkage=0.001,
            bag.fraction=0.5,cv.folds=4,keep.data=TRUE)
?gbm()
fit1 <- gbm( Crop_Damage ~Estimated_Insects_Count + Crop_Type + Soil_Type + Pesticide_Use_Category
             +Number_Doses_Week + Number_Weeks_Used +Number_Weeks_Quit+Season,
             data=learning,distribution="multinomial",n.trees=3000,shrinkage=0.001,
             bag.fraction=0.5,cv.folds=4,keep.data=TRUE )

