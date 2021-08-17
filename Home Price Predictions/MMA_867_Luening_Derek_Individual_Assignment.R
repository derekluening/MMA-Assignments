###########Reading in Libraries###########
library(ggplot2)
library(dplyr)
library(tidyverse)
library(car)
library(caret)
library(mice)
library(corrplot)
library(Metrics)
library(glmnet)

############Reading in our datasets########### 
train_path <- "\\Users\\derek\\OneDrive\\Desktop\\Data Sources\\MMA 867 Datasources\\Individual Assignment\\train.csv"
test_path <- "\\Users\\derek\\OneDrive\\Desktop\\Data Sources\\MMA 867 Datasources\\Individual Assignment\\test.csv"

train <- read.csv(file.choose(), stringsAsFactors = FALSE, header = TRUE, sep = ',')
test<- read.csv(file.choose(), stringsAsFactors = FALSE, header = TRUE, sep = ',')

###########Preliminary investigation of our test dataset###########
str(train) #Lots of variables are set as ints and chr. Will likely need to change most if not all characters to factors at some point.
head(train)#Little difficult to digest what is going on here. 
summary(train) #Seeing some NA values throughout the dataset. We will need to deal with these however, they might be more ordinal/categorical than anything.
md.pattern(train) #Confirmed thoughts above - will need to deal with some missing information 

###########Data Cleaning for our Training Set###########

#Identifying which columns have NA values and how many there are in our training set 
na_columns <- which(colSums(is.na(train)) > 0)
sort(colSums(sapply(train[na_columns], is.na)), decreasing = TRUE) #main columns that have Na's are with the garage and basement, will need to further investigate what is happening here


###Cleaning Pool NA's###

#It appears like there are two pool variables that we need to consider, pool area and quality
#Based on the data dictionary, it looks like the NA's are homes that don't have a pool. Lets recode those to none
train$PoolQC[is.na(train$PoolQC)] <- 'None' #all homes without a pool that were previously NA are recoded to None

###CLEANING FENCE###

#From the data dictionary, the NA's indicate that there is no fence. Changing NA's to No Fence
train$Fence[is.na(train$Fence)] <- 'None'#All fences converted

###Cleaning Fireplace Quality###

#Similar scenario to our fences - Na's indicate that no fireplace exists. Changing Na's to None
train$FireplaceQu[is.na(train$FireplaceQu)] <- 'None'#All values converted

###Cleaning MiscFeature###

#Based on the data dictionary, it appears that Na's represent No misc feature
#Similar scenario to our fireplace and fence, convert Na to None
train$MiscFeature[is.na(train$MiscFeature)] <- 'None' #All values converted

###Cleaning Alley###

#Based on data dictionary, it appears that Na's represent no alley access
#Similar to our fireplace and fence, we will convert our NA to None
train$Alley[is.na(train$Alley)] <- 'None' #All values converted

###Cleaning Electrical###

#We have one missing value in electrical as a result we will code it to the most common value
table(train$Electrical) #SBrKr seems to be the most common 
train$Electrical[is.na(train$Electrical)] <-  'SBrkr'

###Cleaning the MasVNRType and MASVNR Area###

table(train$MasVnrType)

#Based on the data dictionary, if there is a missing value it means that there is no masonry
#on the house. Should recode this to None and the area to 0
train$MasVnrType[is.na(train$MasVnrType)] <-  'None'#Values changed to none
train$MasVnrArea[is.na(train$MasVnrArea)] <-  0 #values changed to 0

###Cleaning our Basement Variables###
table(train$BsmtExposure)
table(train$BsmtFinType2)
table(train$BsmtCond)
table(train$BsmtQual)

#Changing the rest of the categorical variables from NA to None
train$BsmtExposure[is.na(train$BsmtExposure)] <-  'None'
train$BsmtFinType2[is.na(train$BsmtFinType2)] <-  'None'
train$BsmtQual[is.na(train$BsmtQual)] <-  'None'
train$BsmtCond[is.na(train$BsmtCond)] <-  'None'
train$BsmtFinType1[is.na(train$BsmtFinType1)] <- 'None'

###Cleaning our Garage Variables###
table(train$GarageCond)
table(train$GarageQual)
table(train$GarageFinish)

train$GarageType[is.na(train$GarageType)] <-  'None'
train$GarageFinish[is.na(train$GarageFinish)] <-  'None'
train$GarageQual[is.na(train$GarageQual)] <-  'None'
train$GarageCond[is.na(train$GarageCond)] <-  'None'

#Year Garage was built. Assigned this to the year the house was build
train$GarageYrBlt[is.na(train$GarageYrBlt)] <- train$YearBuilt[is.na(train$GarageYrBlt)]

###Cleaning Lot Frontage###
train$LotFrontage[is.na(train$LotFrontage)] <- median(train$LotFrontage,na.rm = T)

###Cleaning the Functional Column###
#There is only one value missing in the functional column, will impute with the mode
table(train$Functional)
train$Functional[is.na(train$Functional)] <-  "Typ"

#Cleaning Exterior1st and Exterior 2nd columns###
#Only one missing value for exterior 1st, will impute with the mode
table(train$Exterior1st)
train$Exterior1st[is.na(train$Exterior1st)] <-  "VinylSd"

#Only one missing value for exterior2nd. Will impute the mode
table(train$Exterior2nd)
train$Exterior2nd[is.na(train$Exterior2nd)] <-  "VinylSd"

###Cleaning the Kitchen Quality Column###
#Only one missing value for kitchen quality. Will impute using the mode
table(train$KitchenQual)
train$KitchenQual[is.na(train$KitchenQual)] <-  "TA"

###Cleaning the sale type column###
#Only one missing value, will impute with the mode
table(train$SaleType)
train$SaleType[is.na(train$SaleType)] <-  "WD"



###########Data Cleaning for our Testing Set###########

#Identifying which columns have NA values and how many there are in our training set 
na_columns <- which(colSums(is.na(test)) > 0)
sort(colSums(sapply(test[na_columns], is.na)), decreasing = TRUE) 

###Cleaning Pool NA's###

#It appears like there are two pool variables that we need to consider, pool area and quality
#Based on the data dictionary, it looks like the NA's are homes that don't have a pool. Lets recode those to none
test$PoolQC[is.na(test$PoolQC)] <- 'None' #all homes without a pool that were previously NA are recoded to None

###Cleaning MiscFeature###

#Based on the data dictionary, it appears that Na's represent No misc feature
#similar scenario to our fireplace and fence, convert Na to None
test$MiscFeature[is.na(test$MiscFeature)] <- 'None' #All values converted

###CLEANING FENCE###

#From the data dictionary, the NA's indicate that there is no fence. Changing NA's to No Fence
test$Fence[is.na(test$Fence)] <- 'None'#All fences converted

###Cleaning Fireplace Quality###

#Similar scenario to our fences - Na's indicate that no fireplace exists. Changing Na's to None
test$FireplaceQu[is.na(test$FireplaceQu)] <- 'None'#All values converted

###Cleaning Alley###

#Based on data dictionary, it appears that Na's represent no alley access
#similar to our fireplace and fence, we will convert our NA to None
test$Alley[is.na(test$Alley)] <- 'None' #All values converted

###Cleaning Masonry type and vnr###
#Based on the data dictionary, if there is a missing value it means that there is no masonary
#on the house. Should recode this to None and the area to 0
test$MasVnrType[is.na(test$MasVnrType)] <-  'None'#Values changed to none
test$MasVnrArea[is.na(test$MasVnrArea)] <-  0 #values changed to 0

###Cleaning Basement Variables###

#Changing the rest of the categorical variables from NA to None and converting area variables to 0
test$BsmtExposure[is.na(test$BsmtExposure)] <-  'None'
test$BsmtFinType2[is.na(test$BsmtFinType2)] <-  'None'
test$BsmtQual[is.na(test$BsmtQual)] <-  'None'
test$BsmtCond[is.na(test$BsmtCond)] <-  'None'
test$BsmtFinType1[is.na(test$BsmtFinType1)] <-  'None'
test$BsmtFinSF1[is.na(test$BsmtFinSF1)] <-  0
test$BsmtFinSF2[is.na(test$BsmtFinSF2)] <-  0
test$TotalBsmtSF[is.na(test$TotalBsmtSF)] <-  0 
test$BsmtUnfSF[is.na(test$BsmtUnfSF)] <-  0 
test$BsmtFullBath[is.na(test$BsmtFullBath)] <-  0
test$BsmtHalfBath[is.na(test$BsmtHalfBath)] <-  0


###Cleaning our Garage Variables###

#Converted NA's to None and area variables to 0 
test$GarageType[is.na(test$GarageType)] <-  'None'
test$GarageFinish[is.na(test$GarageFinish)] <-  'None'
test$GarageQual[is.na(test$GarageQual)] <-  'None'
test$GarageCond[is.na(test$GarageCond)] <-  'None'
test$GarageArea[is.na(test$GarageArea)] <-  0
test$GarageCars[is.na(test$GarageCars)] <-  0

#Cleaning the year that the garages were built column 
#Assigned the year the garage was built to the year the house was built
test$GarageYrBlt[is.na(test$GarageYrBlt)] <- test$YearBuilt[is.na(test$GarageYrBlt)]

###Cleaning Lot Frontage###
test$LotFrontage[is.na(test$LotFrontage)] <- median(test$LotFrontage,na.rm = T)

###Cleaning the Functional Column###
#There is only one value missing in the functional column, will impute with the mode
test$Functional[is.na(test$Functional)] <-  "Typ"

#Cleaning Exterior1st and Exterior 2nd columns###
#Only one missing value for exterior 1st, will impute with the mode
table(test$Exterior1st)
test$Exterior1st[is.na(test$Exterior1st)] <-  "VinylSd"

#Only one missing value for exterior2nd. Will impute the mode
table(test$Exterior2nd)
test$Exterior2nd[is.na(test$Exterior2nd)] <-  "VinylSd"

###Cleaning the Kitchen Quality Column###
#Only one missing value for kitchen quality. Will impute using the mode
table(test$KitchenQual)
test$KitchenQual[is.na(test$KitchenQual)] <-  "TA"

###Cleaning the sale type column###
#Only one missing value, will impute with the mode
table(test$SaleType)
test$SaleType[is.na(test$SaleType)] <-  "WD"

#Cleaning MSZoning
#Only one missing value, will impute with the mode
table(test$MSZoning)
test$MSZoning[is.na(test$MSZoning)] <-  "RL"

#Cleaning Utilities
#Only one missing value, will impute with the mode
table(test$Utilities)
test$Utilities[is.na(test$Utilities)] <-  'AllPub'

#Final missing values check
#No missing values - Ok to proceed to modeling. 
md.pattern(train)
md.pattern(test)

###########Converting characters to factors###########
#Converting Train dataset to Factor
str(train)
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)], as.factor)
str(train)

#Converting additional Train columns to factors that were previously int or num
train$MSSubClass <- as.factor(train$MSSubClass)
train$YearBuilt <- as.factor(train$YearBuilt)
train$MoSold <- as.factor(train$MoSold)
train$YrSold <- as.factor(train$YrSold)
train$GarageYrBlt <- as.factor(train$GarageYrBlt)

#Converting Test dataset to Factor
str(test)
test[sapply(test, is.character)] <- lapply(test[sapply(test, is.character)], as.factor)
str(test)

#Converting additional columns to factors that were previously int or num
test$MSSubClass <- as.factor(test$MSSubClass)
test$YearBuilt <- as.factor(test$YearBuilt)
test$MoSold <- as.factor(test$MoSold)
test$YrSold <- as.factor(test$YrSold)
test$GarageYrBlt <- as.factor(test$GarageYrBlt)

###########Combining Train and Test sets into one Master File###########
test$SalePrice<-rep(NA,1459)
master<- rbind(train,test)

###########Basic Feature Engineering###########
#Creating a column that includes total bathrooms for the entire house and penalized the half baths by 0.5
master$TotalBathrooms <- master$BsmtFullBath + (master$BsmtHalfBath*0.5) + master$FullBath + (master$HalfBath*0.5)

#Creating a column that includes total square footage for the entire house
master$TotalSq <- master$GrLivArea + master$TotalBsmtSF

#Creating a column that indicates if a house was remodeled or not
master$Remodel <- ifelse(master$YearBuilt==master$YearRemodAdd,0,1) #0 = No remodel, 1 = Remodel

###########Building Model train, test, and prediction data sets###########
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
master_predict <- subset(master, Id>=1461)


###########Final Model:Model 21 - MAPE of ~8.43.###########
#This model was submitted to the leaderboard and placed 2297/8952 = 25.6%
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood*TotalSq+LotArea+log(LotArea)+
                  Street+ BldgType*log(GrLivArea)*TotalBsmtSF + 
                  OverallQual+OverallCond +
                  +MSZoning*CentralAir*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)*log(TotalSq)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+TotalBathrooms*GrLivArea*TotalBsmtSF+HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)*log(GrLivArea)+
                  RoofStyle+MasVnrArea+GarageType+GarageArea+GarageQual+PoolQC+MoSold+CentralAir+
                  GarageType*GarageArea+Foundation+ExterQual+BsmtFinType1+BsmtFinSF1+BsmtFinType1*BsmtFinSF1*BsmtFinSF2+BsmtFinSF2+
                  ExterCond+Remodel*log(TotalSq)*CentralAir*HeatingQC+HeatingQC+Remodel+Condition1+SaleCondition*log(TotalSq)*YrSold+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
#write.csv(lasso.predicting, file = "MMA 867 Model 21.csv")

########Other Models that were tried and tested but not used#######

###Model 1###
y<-log(master_train$SalePrice)
X<-model.matrix(Id~SaleCondition+SaleType+YrSold+MoSold
                +MiscVal+MiscFeature+Fence+PoolQC+PoolArea+ScreenPorch+EnclosedPorch
                +OpenPorchSF+WoodDeckSF+PavedDrive+GarageCond+GarageQual+GarageArea+GarageCars+GarageFinish+GarageYrBlt
                +GarageType+FireplaceQu+Fireplaces+Functional+TotRmsAbvGrd+KitchenQual+KitchenAbvGr+BedroomAbvGr+HalfBath
                +FullBath+BsmtHalfBath+BsmtFullBath+GrLivArea+LowQualFinSF+X2ndFlrSF+X1stFlrSF+Electrical
                +CentralAir+HeatingQC+Heating+TotalBsmtSF+BsmtUnfSF+BsmtFinSF2+BsmtFinType2+BsmtFinSF1
                +BsmtFinType1+BsmtExposure+BsmtCond+BsmtQual+Foundation+ExterCond+ExterQual+MasVnrArea+MasVnrType
                +Exterior2nd+Exterior1st+RoofMatl+RoofStyle+YearRemodAdd+YearBuilt+OverallCond+OverallQual+HouseStyle
                +BldgType+Condition2+Condition1+Neighborhood+LandSlope+LotConfig+Utilities+LandContour+LotShape+Alley
                +Street+LotArea+LotFrontage+MSZoning+MSSubClass, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 1.csv") 



###Mode2 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(log(Id)~ Neighborhood+ log(LotArea)+ BldgType + OverallQual + OverallCond +
                  YearBuilt + TotalBsmtSF + GrLivArea + BsmtFullBath + BsmtHalfBath + 
                  FullBath + HalfBath + BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 2.csv") 

###Model 3###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ OverallQual + GrLivArea + OverallQual*GrLivArea + GarageArea + TotalBsmtSF+
                  YearBuilt * GrLivArea + Neighborhood,master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 3.csv") 



### Model 4 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~SaleCondition+SaleType+YrSold+MoSold
                +MiscVal+MiscFeature+Fence+PoolQC+PoolArea+ScreenPorch+EnclosedPorch
                +OpenPorchSF+WoodDeckSF+PavedDrive+GarageCond+GarageQual+GarageArea+GarageCars+GarageFinish+GarageYrBlt
                +GarageType+FireplaceQu+Fireplaces+Functional+TotRmsAbvGrd+KitchenQual+KitchenAbvGr+BedroomAbvGr+HalfBath
                +FullBath+BsmtHalfBath+BsmtFullBath+GrLivArea*YearBuilt*OverallQual+LowQualFinSF+X2ndFlrSF+X1stFlrSF+Electrical
                +CentralAir+HeatingQC+Heating+TotalBsmtSF*YearBuilt+BsmtUnfSF*YearBuilt+BsmtFinSF2*YearBuilt+BsmtFinType2+BsmtFinSF1*YearBuilt
                +BsmtFinType1+BsmtExposure+BsmtCond+BsmtQual+Foundation+ExterCond+ExterQual+MasVnrArea+MasVnrType
                +Exterior2nd+Exterior1st+RoofMatl+RoofStyle+YearRemodAdd+YearBuilt+OverallCond+OverallQual+HouseStyle+HouseStyle*OverallQual*GrLivArea*TotalBsmtSF*Neighborhood*YearBuilt
                +BldgType+Condition2+Condition1+Neighborhood+LandSlope+LotConfig+Utilities+LandContour+LotShape+Alley
                +Street+LotArea+LotFrontage+MSZoning+MSSubClass, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 4.csv") 


### Model 5 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master$SalePrice)
X <- model.matrix(Id~OverallQual+GrLivArea+GarageArea+TotalBsmtSF+FullBath+TotRmsAbvGrd+
                  YearBuilt+YearRemodAdd, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 5.csv") 




###Model 5 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ Neighborhood+ log(LotArea)*OverallQual+ log(LotArea)+LotArea+ BldgType + OverallQual + OverallCond +
                  YearBuilt + TotalSq+ TotalBathrooms + BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 2.csv") 



###Model 6 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ Neighborhood+ log(TotalSq)+TotalSq+ Neighborhood*TotalSq+TotalSq*TotalBathrooms+TotalBathrooms+ BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 6.csv") 

###Model 7###

master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass*log(TotalSq)+TotalSq+Neighborhood+log(TotalSq)*Neighborhood+YearBuilt+log(TotalSq)*OverallCond+TotalBathrooms+
                  GarageArea+Remodel+MoSold+BsmtCond+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 7.csv") 

str(master)

###MODEL 8###
fit.log.step <- step(lm(log(SalePrice)~ SaleCondition+SaleType+YrSold+MoSold
                        +MiscVal+MiscFeature+Fence+PoolQC+PoolArea+ScreenPorch+EnclosedPorch
                        +OpenPorchSF+WoodDeckSF+PavedDrive+GarageCond+GarageQual+GarageArea+GarageCars+GarageFinish+GarageYrBlt
                        +GarageType+FireplaceQu+Fireplaces+Functional+TotRmsAbvGrd+KitchenQual+KitchenAbvGr+BedroomAbvGr+HalfBath
                        +FullBath+BsmtHalfBath+BsmtFullBath+GrLivArea+LowQualFinSF+X2ndFlrSF+X1stFlrSF+Electrical
                        +CentralAir+HeatingQC+Heating+TotalBsmtSF+BsmtUnfSF+BsmtFinSF2+BsmtFinType2+BsmtFinSF1
                        +BsmtFinType1+BsmtExposure+BsmtCond+BsmtQual+Foundation+ExterCond+ExterQual+MasVnrArea+MasVnrType
                        +Exterior2nd+Exterior1st+RoofMatl+RoofStyle+YearRemodAdd+YearBuilt+OverallCond+OverallQual+HouseStyle
                        +BldgType+Condition2+Condition1+Neighborhood+LandSlope+LotConfig+Utilities+LandContour+LotShape+Alley
                        +Street+LotArea+LotFrontage+MSZoning+MSSubClass+TotalSq+TotalBathrooms+Remodel,master_train), direction='both')



master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ BsmtFinSF2+BsmtFinSF1+BedroomAbvGr+BsmtHalfBath+TotalBathrooms 
                +Remodel+Street+LowQualFinSF+X1stFlrSF+GarageCars+EnclosedPorch 
                + OpenPorchSF+MasVnrType+PoolArea+MiscVal+BsmtQual+Alley+
                  LotShape+GarageFinish+BsmtExposure+ExterCond+BldgType+PoolQC+
                  BsmtFinType2+Electrical+BsmtCond+ExterQual+MiscFeature+BsmtFinType1 
                + RoofStyle+GarageQual+GarageCond+FireplaceQu+GarageType 
                + Exterior2nd+MoSold+YearBuilt+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 8.csv") 


###Model 9###
fit33 <- lm(SalePrice ~ TotalSq*BldgType+ BedroomAbvGr+TotalBathrooms+TotalBathrooms*TotalSq + Neighborhood+Street*BldgType + BldgType, master_train)
test.fit33 <- predict(fit33, master_test)
percent.errors <- abs((master_test$SalePrice-test.fit33)/master_test$SalePrice)*100 
mean(percent.errors)

###model 11###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ OverallQual+GrLivArea+log(GrLivArea)+OverallQual*log(GrLivArea)+
                TotalBathrooms+YearBuilt+OverallCond+log(GrLivArea)*OverallCond+TotRmsAbvGrd+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(master_test$SalePrice-lasso.testing/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 11.csv") 

master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ OverallQual+GrLivArea+log(GrLivArea)+OverallQual*log(GrLivArea)+GarageArea
                +GarageQual*GarageCond*GarageArea+TotalBsmtSF+YearBuilt+GarageFinish+GarageFinish*GarageYrBlt+
                  GarageYrBlt+ExterQual+KitchenQual+
                  +TotRmsAbvGrd+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(master_test$SalePrice-lasso.testing/master_test$SalePrice*100))

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 10.csv") 

###Mode12 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ Neighborhood+LotArea+log(LotArea)+Street+ BldgType + OverallQual + OverallCond +
                  +Neighborhood*BldgType*OverallQual*OverallCond+YearBuilt +TotalBsmtSF+ log(GrLivArea)+ 
                  GrLivArea+BsmtFullBath + BsmtHalfBath + 
                  FullBath+GarageType+GarageArea+GarageQual+PoolQC+MoSold+
                  GarageType*GarageArea+ExterQual+ ExterCond+HalfBath + BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 12.csv") 


###Mode13 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood+LotArea+log(LotArea)+
                  Street+ BldgType + 
                  OverallQual+OverallCond +
                  +MSZoning*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+BsmtFullBath + BsmtHalfBath +HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)+
                  RoofStyle+MasVnrArea+FullBath+GarageType+GarageArea+GarageQual+PoolQC+MoSold+
                  GarageType*GarageArea+ExterQual+ExterCond+HalfBath+Remodel+ BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 13.csv") 

###Mode14###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ Neighborhood*log(TotalSq)+Neighborhood+log(TotalSq)+TotalSq+TotalBathrooms+YearBuilt+MoSold+
                BsmtCond+BedroomAbvGr+GarageType+MSZoning+LotArea+HouseStyle, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 13.csv") 



###Mode15###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ OverallQual*YearBuilt+log(GrLivArea)+GrLivArea+GarageCars+TotalBsmtSF+FullBath+TotRmsAbvGrd+YearBuilt
                +Remodel+MasVnrArea+Fireplaces+BsmtFinSF1, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 13.csv") 

###Mode16 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood*TotalSq+LotArea+log(LotArea)+
                  Street+ BldgType*log(GrLivArea)*TotalBsmtSF + 
                  OverallQual+OverallCond +
                  +MSZoning*CentralAir*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)*log(TotalSq)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+TotalBathrooms*GrLivArea*TotalBsmtSF+HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)*log(GrLivArea)+
                  RoofStyle+MasVnrArea+GarageType+GarageArea+GarageQual+PoolQC+MoSold+CentralAir+
                  GarageType*GarageArea+Foundation+ExterQual+BsmtFinType1+BsmtFinSF1+BsmtFinType1*BsmtFinSF1*BsmtFinSF2+BsmtFinSF2+
                  ExterCond+Remodel*log(TotalSq)+Remodel+Condition1+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
#write.csv(lasso.predicting, file = "MMA 867 Model 16.csv") 

###Mode17 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood*TotalSq+LotArea+log(LotArea)+
                  Street+ BldgType*log(GrLivArea)*TotalBsmtSF + 
                  OverallQual+OverallCond +
                  +MSZoning*CentralAir*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)*log(TotalSq)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+TotalBathrooms*GrLivArea*TotalBsmtSF+HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)*log(GrLivArea)+
                  RoofStyle+MasVnrArea+GarageType+GarageArea+GarageQual+PoolQC+MoSold+CentralAir+
                  GarageType*GarageArea+Foundation+ExterQual+BsmtFinType1+BsmtFinSF1+BsmtFinType1*BsmtFinSF1*BsmtFinSF2+BsmtFinSF2+
                  ExterCond+Remodel*log(TotalSq)*CentralAir+Remodel+Condition1+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 17.csv")

###Mode18 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood*TotalSq+LotArea+log(LotArea)+
                  Street+ BldgType*log(GrLivArea)*TotalBsmtSF + 
                  OverallQual+OverallCond +
                  +MSZoning*CentralAir*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)*log(TotalSq)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+TotalBathrooms*GrLivArea*TotalBsmtSF+HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)*log(GrLivArea)+
                  RoofStyle+MasVnrArea+GarageType+GarageArea+GarageQual+PoolQC+MoSold+CentralAir+
                  GarageType*GarageArea+Foundation+ExterQual+BsmtFinType1+BsmtFinSF1+BsmtFinType1*BsmtFinSF1*BsmtFinSF2+BsmtFinSF2+
                  ExterCond+Remodel*log(TotalSq)*CentralAir+Remodel+Condition1+SaleCondition+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 18.csv") 

###Mode19 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood*TotalSq+LotArea+log(LotArea)+
                  Street+ BldgType*log(GrLivArea)*TotalBsmtSF + 
                  OverallQual+OverallCond +
                  +MSZoning*CentralAir*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)*log(TotalSq)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+TotalBathrooms*GrLivArea*TotalBsmtSF+HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)*log(GrLivArea)+
                  RoofStyle+MasVnrArea+GarageType+GarageArea+GarageQual+PoolQC+MoSold+CentralAir+
                  GarageType*GarageArea+Foundation+ExterQual+BsmtFinType1+BsmtFinSF1+BsmtFinType1*BsmtFinSF1*BsmtFinSF2+BsmtFinSF2+
                  ExterCond+Remodel*log(TotalSq)*CentralAir+Remodel+Condition1+SaleCondition*log(TotalSq)+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
#write.csv(lasso.predicting, file = "MMA 867 Model 19.csv") 

###Model 20 ###
master_train <- subset(master, Id<=1000)
master_test <- subset(master, Id>=1001 & Id <=1460)
y<-log(master_train$SalePrice)
X<-model.matrix(Id~ MSSubClass+Neighborhood+LotConfig+MSZoning+MSZoning*LotConfig*LotArea*Neighborhood*TotalSq+LotArea+log(LotArea)+
                  Street+ BldgType*log(GrLivArea)*TotalBsmtSF + 
                  OverallQual+OverallCond +
                  +MSZoning*CentralAir*Neighborhood*BldgType*OverallQual*OverallCond*log(OverallQual)*log(TotalSq)+YearBuilt +TotalBsmtSF+
                  TotalBsmtSF*BsmtCond+BsmtCond+log(GrLivArea)+ 
                  GrLivArea+TotalBathrooms*GrLivArea*TotalBsmtSF+HouseStyle+ HouseStyle*Neighborhood*LotArea*log(LotArea)*log(GrLivArea)+
                  RoofStyle+MasVnrArea+GarageType+GarageArea+GarageQual+PoolQC+MoSold+CentralAir+
                  GarageType*GarageArea+Foundation+ExterQual+BsmtFinType1+BsmtFinSF1+BsmtFinType1*BsmtFinSF1*BsmtFinSF2+BsmtFinSF2+
                  ExterCond+Remodel*log(TotalSq)*CentralAir+Remodel+Condition1+SaleCondition*log(TotalSq)*YrSold+BedroomAbvGr, master)[,-1]
X<-cbind(master$Id,X)
X.training<-subset(X,X[,1]<=1000)
X.testing<-subset(X, (X[,1]>=1001 & X[,1]<=1460))
X.predicting<-subset(X, (X[,1]>=1461))

lasso.fit <- glmnet(x = X.training, y=y, alpha = 1)
plot(lasso.fit, xvar = 'lambda')

crossval <- cv.glmnet (x=X.training, y=y, alpha =1)
plot(crossval)

penalty.lasso <- crossval$lambda.min
log(penalty.lasso)

lasso.opt.fit <- glmnet(x = X.training, y=y, alpha=1, lambda = penalty.lasso)
coef(lasso.opt.fit)

lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.testing))
mean(abs(lasso.testing-master_test$SalePrice)/master_test$SalePrice)*100

lasso.predicting <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx = X.predicting))
write.csv(lasso.predicting, file = "MMA 867 Model 20.csv")




