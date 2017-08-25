library(ggplot2)
library(gplots)
library(caTools)
library(lattice)
library(caret)
library(foreach)
library(Matrix)
library(pROC)
library(ROCR)
library(Rcpp)
library(mice)
library(xgboost)
library(survival)
library(gbm)
library(randomForest)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
require(magrittr)
library(h2o)
library(Ckmeans.1d.dp)

#data preprocessed in excel
# training file :Train_seers_accuracy_2003-2005_v0.2.csv
# testing file :  Train_seers_accuracy2003_2006_v0.1.csv
#read train
learn_seer_av<-read.csv(file.choose())
str(learn_seer_av)
learn_seer_av[,c(44)]  = lapply(c(44), function(x) as.factor(learn_seer_av[,x]))
test_seer_av<-read.csv(file.choose())
#samplesub's first column is client ID column
samplesub <- as.matrix(test_seer_av$Client_ID)
names(samplesub) <- c("Client_ID")
#Find the column types
lapply(learn_seer_av, class)

#Create a separate label data
label <- data.frame(label=learn_seer_av[,ncol(learn_seer_av)])

#Find frequencies of the labl
table(label)

#################
##remove Cross_Sell column
learn_seer_a1 <- data.frame(learn_seer_av[, -ncol(learn_seer_av)])
test_seer_a1 <- data.frame(test_seer_av[, -ncol(test_seer_av)])

################
#using h2o
localH2O = h2o.init()
h2o.shutdown()
h2o.init(nthreads=-1,max_mem_size='6G')

#Drop the age column
learn_seer_av3 <- learn_seer_av[,-2] #remove age
test_seer_av2<-test_seer_a1[,-2]     #remove age
str(learn_seer_av3)
str(test_seer_av2)
## Load data into cluster from R
trainHex1<-as.h2o(learn_seer_av3)
summary(learn_seer_av3$Cross_Sell)
## Set up variable to use all features other than those specified here
features1<-names(learn_seer_av3[,c(1:42)])
## Load test data into cluster from R
testHex1<-as.h2o(test_seer_av2)

set.seed(27)
fit1gbmx1 <- h2o.gbm( x=features1,y="Cross_Sell",
                     distribution="bernoulli",ntrees=1000,
                     nfolds=4,max_depth = 10,stopping_rounds = 1,
                     score_each_iteration = F,learn_rate = 0.02,
                     stopping_tolerance = 0,
                     col_sample_rate=0.7, sample_rate = 0.5,
                     training_frame=trainHex1)

summary(fit1gbmx1)
predictgbmxy1 <- as.data.frame(h2o.predict(fit1gbmx1,testHex1))
head(samplesub)
#Predict_testlms3 <- predict(fitlearnlms3, TestDatalms,type = "class")
submitg5 <- data.frame(Client_ID = samplesub,
                       Cross_Sell = predictgbmxy1$p1)
write.csv(submitg5, file = "av_gbm_seerxy2.csv", row.names = FALSE)


#################
##using xgboost##
##keeping client id
learn_seer_a2 <- data.frame(learn_seer_a1[, -2])     #remove age
test_seer_a2 <- data.frame(test_seer_a1[, -2]) #remove age

str(learn_seer_a2)
learn_seer_a2[,c(1:42)]  = lapply(c(1:42),function(x) as.numeric(learn_seer_a2[,x]))
test_seer_a2[,c(1:42)]  = lapply(c(1:42),function(x) as.numeric(test_seer_a2[,x]))
######################################
#xgboost
######################################
dtrain2 <- xgb.DMatrix(data=as.matrix(learn_seer_a2), label=as.matrix(label[,1]))
watchlist=list(train=dtrain2)
set.seed(109)
model_xgboost_109 <- xgboost(data=dtrain2,
                             max.depth=20,
                             eta=0.1,
                             gamma=3,
                             min_child_weight=6,
                             subsample=0.9,
                             colsample_bytree=0.5,
                             nround=500,
                             nthread=4,
                             objective="binary:logistic",
                             verbose=1,
                             watchlist=watchlist,
                             eval.metric="logloss",
                             set.seed=108
)

# Get the feature real names
names <- dimnames(learn_seer_a2)[[2]]
predtesta1 <- data.frame(predict(model_xgboost_109, as.matrix(test_seer_a2)))
table(predtesta1)

######################################
#End of xgboost model
######################################
#create sample submission
samplesuba2 <- data.frame(cbind(samplesub, predtesta1))
names(samplesuba2) <- c("Client_ID", "Cross_Sell")
write.csv(samplesuba2, file = "av_submits_seera2.csv", row.names = FALSE)

################
##ensembled 0.75 * predictgbmxy1$p1 + 0.25 * predtesta1
final_sub <-data.frame(cbind(samplesub,(0.75 * predictgbmxy1$p1 + 0.25 * predtesta1)))
names(final_sub) <- c("Client_ID", "Cross_Sell")
write.csv(final_sub, file = "ensmbl5.csv", row.names = FALSE)
