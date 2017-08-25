library(caret)
library(data.table)
library(Matrix)
library(xgboost)
library(caret)
library(dplyr)
library(corrplot)
library(gbm)
require(dplyr)

# Load CSV files

data.train <- read.csv(file.choose(),na.strings=c(""," ","NA"))
data.test <- read.csv(file.choose())

data.train<-as.data.table(data.train)
data.test<-as.data.table(data.test)

str(data.train)
summary(data.test)

# Combine data.tables
data <- rbind(data.train, data.test,fill=TRUE)

data<-as.data.table(data)
str(data)

#Total_Accounts and Number_Open_Accounts are highly correlated so remove one to avoid multicollinearity
data[,c("Total_Accounts")
     :=NULL]

features = colnames(data)
for (f in features){
  if( (class(data[[f]]) == "character") || (class(data[[f]]) == "factor"))
  {
    levels = unique(data[[f]])
    data[[f]] = as.numeric(factor(data[[f]], level = levels))
  }
}
Interest_Rate<-data$Interest_Rate
data3<-data
data[,c("Interest_Rate")
     :=NULL]
data[is.na(data)] = -1
str(data)
data<-cbind(data,Interest_Rate)


# Some relative features
data[,":="(openacc_totacc_ratio = Number_Open_Accounts/Total_Accounts
)]

data[,c("Number_Open_Accounts","Total_Accounts")
     :=NULL]

str(data)
########## DATA PREPROCESSING END #########

####### ONE HOT ENCODING ##########
data[, Gender:=as.factor(Gender)]
data[, Area_Type := as.factor(Area_Type)]
data[, Loan_Grade:=as.factor(Loan_Grade)]
data[, Length_Employed:=as.factor(Length_Employed)]
data[, Home_Owner:=as.factor(Home_Owner)]
data[, Income_Verified:=as.factor(Income_Verified)]
data[, Purpose_Of_Loan:=as.factor(Purpose_Of_Loan)]
data[, Interest_Rate:=as.factor(Interest_Rate)]

data2<-data
data<-data2
# one-hot-encoding features
data = as.data.frame(data)
ohe_feats = c( 'Gender','Area_Type', 'Loan_Grade', 'Length_Employed', 'Home_Owner', 'Income_Verified','Purpose_Of_Loan')
dummies = dummyVars(~ Gender+ Area_Type + Loan_Grade + Length_Employed + Home_Owner + Income_Verified + Purpose_Of_Loan , data = data)
df_all_ohe <- as.data.frame(predict(dummies, newdata = data))
df_all_combined <- cbind(data[,-c(which(colnames(data) %in% ohe_feats))],df_all_ohe)
data = as.data.table(df_all_combined)
str(data)



#seperate train and test data
data.train1<-data[!is.na(Interest_Rate)]

data.train1$Interest_Rate<-as.factor(data.train1$Interest_Rate)
str(data.train1)
data.test1<-data[is.na(Interest_Rate)]
str(data.test1)
y_train<-data.train1$Interest_Rate
test_ids<-data.test1$Loan_ID

data.train2<-as.data.frame(data.train1)

data.train2<-data.train2[,-1]

str(data.train2)
data.test2<-as.data.frame(data.test1)

data.test2<-data.test2[,-7]
data.test2<-data.test2[,-1]
str(data.test2)


###############final XGB #############
data.train3<-data.train2

#Create a separate label data
label <- data.frame(label=data.train3[,6])

data.train3<-data.train3[,-6]


str(data.train3)

dtrain <- xgb.DMatrix(data=as.matrix(data.train3), label=as.matrix(label[,1]))
watchlist=list(train=dtrain)

dtest <- xgb.DMatrix(data=as.matrix(data.test2));

gc()
#xgboost
######################################
set.seed(121)
model_xgb_1 <- xgboost(data=dtrain,
                       max.depth=20,
                       eta=0.05,
                       gamma=1,
                       min_child_weight=6,
                       subsample=0.5,
                       colsample_bytree=0.5,
                       base_score=0,nfold=4,
                       nround=200,
                       nthread=6,
                       objective="multi:softmax",
                       num_class=4,
                       verbose=2,
                       watchlist=watchlist,
                       eval.metric="mlogloss",
                       set.seed=14
)


mpreds = data.table(Loan_ID=test_ids)

vpreds = predict(model_xgb_1,dtest)

mpreds = cbind(mpreds, vpreds)
colnames(mpreds)[2] = paste("pred_seed_", 1, sep="")
head(mpreds)


submission = data.table(Loan_ID=test_ids, Interest_Rate=mpreds$pred_seed_1)

write.table(submission, "avloan_submission_versn_04.csv", sep=",", dec=".", quote=FALSE, row.names=FALSE)


##### xgb model ends ##############

