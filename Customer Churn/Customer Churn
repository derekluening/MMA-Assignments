#Part One - Prepare STCdata_A####

###
#1.1 Load STCdata_A####
###

if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, and if needed install the necessary packages
Packages <- c('tidyverse', 'mice', 'readxl', 'MASS','caret','ggplot2', 'tidyr', 'lubridate','lattice', 'car','estimatr')
lapply(Packages, library, character.only = TRUE)
rm(Packages)
STCdata_A <- read.csv(file.choose(), header=TRUE, stringsAsFactors = TRUE)
dim(STCdata_A)
str(STCdata_A) 

####################################START#####################################################################
STCdata_A$SeniorCitizen <- as.factor(STCdata_A$SeniorCitizen)
STCdata_A$Churn_numeric <- ifelse(STCdata_A$Churn == 'Yes', 1,0)
summary(STCdata_A)

#check missing value, DELETE 11 ROWS
md.pattern(STCdata_A)
STCdata_A <- STCdata_A[complete.cases(STCdata_A),]
sum(is.na(STCdata_A$TotalCharges)) 

#CORREALETION
telco_cor <- round(cor(STCdata_A[,c("tenure", "MonthlyCharges", "TotalCharges")]), 5)

###
#1.2 Data exploration####
###

#review the distribution of numeric values and their relationship
hist(STCdata_A$tenure,main="Tenure Histogram",xlab="Tenure",ylab="Count")
STCdata_A %>% ggplot(mapping = aes(x = tenure)) +geom_bar(aes(fill = Churn),alpha=0.5)
cor(STCdata_A$tenure,STCdata_A$Churn_numeric) #negative relationship -0.35

hist(STCdata_A$MonthlyCharges,main="Monthly Chargers Histogram",xlab="Monthly_charges",ylab="Count")
ggplot(STCdata_A, aes(x=MonthlyCharges, y=Churn_numeric)) + geom_point() #more churn are have higher monthly charges
cor(STCdata_A$MonthlyCharges,STCdata_A$Churn_numeric)#positive relationship

hist(STCdata_A$TotalCharges,main="Total Chargers Histogram",xlab="Total_charges",ylab="Count")
ggplot(STCdata_A, aes(x=TotalCharges, y=Churn_numeric)) + geom_point() #more churn are have lower total charges
cor(STCdata_A$TotalCharges,STCdata_A$Churn_numeric) #negative relationship
ggplot(STCdata_A, aes(x=tenure, y=TotalCharges)) + geom_point() #more tenure with more total charges
cor(STCdata_A$tenure,STCdata_A$TotalCharges) #strong postie relationship which is 0.83

ggplot(STCdata_A, aes(x=tenure, y=MonthlyCharges)) + geom_point() 
STCdata_A %>% ggplot(aes(x = MonthlyCharges,fill = Churn)) + geom_bar(aes(fill = Churn),alpha=0.5)
cor(STCdata_A$tenure,STCdata_A$MonthlyCharges) #weak postie relationship which is 0.24

ggplot(STCdata_A, aes(x=TotalCharges, y=MonthlyCharges)) + geom_point() 
cor(STCdata_A$TotalCharges,STCdata_A$MonthlyCharges) # postie relationship which is 0.65

#Churn rate
table(STCdata_A$Churn)
per_churn <- sum(STCdata_A$Churn == "Yes")/nrow(STCdata_A)
per_churn #churn rate around 26.5%

#review the distribution of character values and their relationship
STCdata_A %>% ggplot(mapping = aes(x = gender)) +geom_bar(aes(fill = Churn),alpha=0.5)#both female and male have similar proprtion churn rate

STCdata_A %>% ggplot(mapping = aes(x = SeniorCitizen)) +geom_bar(aes(fill = Churn),alpha=0.5)#more 'not senior citizen' group did not leave

STCdata_A %>% ggplot(mapping = aes(x = Partner)) +geom_bar(aes(fill = Churn),alpha=0.5)

STCdata_A %>% ggplot(mapping = aes(x = Dependents)) +geom_bar(aes(fill = Churn),alpha=0.5)#bigger proportion of dependents group didn't leave

STCdata_A %>% ggplot(mapping = aes(x = PhoneService)) +geom_bar(aes(fill = Churn),alpha=0.5)

STCdata_A %>% ggplot(mapping = aes(x = InternetService)) +geom_bar(aes(fill = Churn),alpha=0.5)#around 90% of 'No internet service' customer no churn

STCdata_A %>% ggplot(mapping = aes(x = PaymentMethod)) +geom_bar(aes(fill = Churn),alpha=0.5)# Electronic charge have more change to churn compare to others


# check for rare categories
table(STCdata_A$gender)
table(STCdata_A$SeniorCitizen)
table(STCdata_A$Partner) 
table(STCdata_A$Dependents) 
table(STCdata_A$tenure)# will do some feature engineering
table(STCdata_A$PhoneService)
table(STCdata_A$MultipleLines)#need to combine
table(STCdata_A$InternetService)
table(STCdata_A$OnlineSecurity)#need to combine
table(STCdata_A$OnlineBackup)#need to combine
table(STCdata_A$DeviceProtection)#need to combine
table(STCdata_A$TechSupport)#need to combine
table(STCdata_A$StreamingTV)#need to combine
table(STCdata_A$StreamingMovies)#need to combine
table(STCdata_A$Contract)
table(STCdata_A$PaperlessBilling)
table(STCdata_A$PaymentMethod)



###
#1.3 Feature engineering####
###

STCdata_A <- data.frame(lapply(STCdata_A, function(x) {gsub("No internet service", "No", x)}))
STCdata_A <- data.frame(lapply(STCdata_A, function(x) {gsub("No phone service", "No", x)}))




STCdata_A <- mutate(STCdata_A, tenure_bin = tenure)
STCdata_A$tenure_bin[STCdata_A$tenure_bin >=0 & STCdata_A$tenure_bin <= 12] <- '0-1 year'
STCdata_A$tenure_bin[STCdata_A$tenure_bin > 12 & STCdata_A$tenure_bin <= 24] <- '1-2 years'
STCdata_A$tenure_bin[STCdata_A$tenure_bin > 24 & STCdata_A$tenure_bin <= 36] <- '2-3 years'
STCdata_A$tenure_bin[STCdata_A$tenure_bin > 36 & STCdata_A$tenure_bin <= 48] <- '3-4 years'
STCdata_A$tenure_bin[STCdata_A$tenure_bin > 48 & STCdata_A$tenure_bin <= 60] <- '4-5 years'
STCdata_A$tenure_bin[STCdata_A$tenure_bin > 60 & STCdata_A$tenure_bin <= 72] <- '5-6 years'

STCdata_A$tenure_bin <- as.factor(STCdata_A$tenure_bin)
str(STCdata_A$tenure_bin)

#creating a total products category
STCdata_A$InternetServiceCount <- ifelse(STCdata_A$InternetService == 'No', 0,1 )
STCdata_A$PhoneServiceCount <- ifelse(STCdata_A$PhoneService == 'No', 0,1 )
STCdata_A$MultipleLinesCount <- ifelse(STCdata_A$MultipleLines == 'Yes', 1,0)
STCdata_A$OnlineSecurityCount <- ifelse(STCdata_A$OnlineSecurity == 'Yes', 1,0)
STCdata_A$OnlineBackupCount <- ifelse(STCdata_A$OnlineBackup == 'Yes', 1,0)
STCdata_A$DeviceProtectionCount <- ifelse(STCdata_A$DeviceProtection == 'Yes', 1,0)
STCdata_A$TechSupportCount <- ifelse(STCdata_A$TechSupport == 'Yes', 1,0)
STCdata_A$StreamingTVCount <- ifelse(STCdata_A$StreamingTV == 'Yes', 1, 0)
STCdata_A$streamingMoviesCount <- ifelse(STCdata_A$StreamingMovies == 'Yes', 1,0)
STCdata_A$TotalProducts <- rowSums(cbind(STCdata_A$InternetServiceCount,STCdata_A$PhoneServiceCount,STCdata_A$MultipleLinesCount,STCdata_A$OnlineSecurityCount,STCdata_A$OnlineBackupCount,STCdata_A$DeviceProtectionCount,
                                    STCdata_A$TechSupportCount,STCdata_A$StreamingTVCount,STCdata_A$streamingMoviesCount))
STCdata_A <- subset(STCdata_A, select = -c(InternetServiceCount,PhoneServiceCount,MultipleLinesCount,OnlineBackupCount,OnlineSecurityCount,DeviceProtectionCount,TechSupportCount,
                                 streamingMoviesCount,StreamingTVCount))
#Creating total product bins to better understand how products are distributed in the STCdata_Aset
min(STCdata_A$TotalProducts)
max(STCdata_A$TotalProducts)
STCdata_A <- mutate(STCdata_A,TotalProductsRange = TotalProducts)
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >=0 & STCdata_A$TotalProductsRange <=1] <- '0-1 product'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >1 & STCdata_A$TotalProductsRange <=2] <-'1-2 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >2 & STCdata_A$TotalProductsRange <=3] <-'2-3 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >3 & STCdata_A$TotalProductsRange <=4] <-'3-4 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >4 & STCdata_A$TotalProductsRange <=5] <-'4-5 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >5 & STCdata_A$TotalProductsRange <=6] <-'5-6 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >6 & STCdata_A$TotalProductsRange <=7] <-'6-7 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >7 & STCdata_A$TotalProductsRange <=8] <-'7-8 products'
STCdata_A$TotalProductsRange[STCdata_A$TotalProductsRange >8 & STCdata_A$TotalProductsRange <=9] <-'8-9 products'
table(STCdata_A$TotalProductsRange)



STCdata_A$MonthlyCharges_Category <- ifelse(STCdata_A$MonthlyCharges <= 40, 'low monthly charge',
                                            ifelse(STCdata_A$MonthlyCharges >= 80, 'high monthly charge', 'medium monthly charge')
                                            )
                 
STCdata_A$TotalCharges_Category <- ifelse(STCdata_A$TotalCharges <= 1000, '<=1000',
                                            ifelse(STCdata_A$MonthlyCharges <= 2000, '1000-2000', 
                                                   ifelse(STCdata_A$MonthlyCharges <= 4000, '2000-4000','>4000')))

num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
STCdata_A[num_columns] <- sapply(STCdata_A[num_columns], as.numeric)
STCdata_A$monthtototal <- STCdata_A$MonthlyCharges/STCdata_A$TotalCharges
sum(is.infinite(STCdata_A$monthtototal))


#Remove ID and churn character 
#telco_int <- STCdata_A[,c("tenure", "MonthlyCharges", "TotalCharges")]
#telco_int <- data.frame(scale(telco_int))

STCdata_A <- select(STCdata_A, -customerID, -Churn,-TotalProducts)
#STCdata_A <- select(STCdata_A, -tenure, -MonthlyCharges, -TotalCharges)
#STCdata_A <- cbind(telco_int,STCdata_A)
STCdata_A[sapply(STCdata_A, is.character)] <- lapply(STCdata_A[sapply(STCdata_A, is.character)], as.factor)
str(STCdata_A)

#standardize continuous vars

#scale(STCdata_A$tenure, center = TRUE, scale = TRUE)

#calculate the initial threshold
STCdata_A$Churn_numeric <- as.numeric(as.character(STCdata_A$Churn_numeric))

threshold <- per_churn
str(STCdata_A)


#1.4 Split testing1870, training4362, verify800####
set.seed(1111) #set a random number generation seed to ensure that the split is the same every time
verifyIndex <- createDataPartition(y = STCdata_A$Churn_numeric, p = 800/7032, list = FALSE, times = 1)
verify <- STCdata_A[verifyIndex,]
test_train <- STCdata_A[-verifyIndex,]
testIndex <- createDataPartition(y = test_train$Churn_numeric, p = 0.3, list = FALSE, times = 1)
testing <- test_train[ testIndex,]
training <- test_train[ -testIndex,]


###
#Part Two - Models####
###

###
#2.1 Lg+AIC AUC0.8500219, profit83430.54####
###
model_logistic <- glm(Churn_numeric ~ . , data=training, family="binomial"(link="logit")) 
summary(model_logistic) 
#stepAIC method and will iterate the lowest AIC model among all models
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) 
summary(model_logistic_stepwiseAIC) 
formula(model_logistic_stepwiseAIC)

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

#predict
logistic_probabilities <- predict(model_logistic_stepwiseAIC,newdata=testing,type="response")
logistic_classification<-rep("1",1870)
logistic_classification[logistic_probabilities< threshold]="0" 
logistic_classification<-as.factor(logistic_classification)
testing$Churn_numeric<-as.factor(testing$Churn_numeric)
levels(logistic_classification)
levels(testing$Churn_numeric)

#Confusion matrix 
confusionMatrix(logistic_classification,testing$Churn_numeric,positive = "1") 

#ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$Churn_numeric)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") 
plot(logistic_ROC) 

#AUC - 0.8500219
auc.tmp <- performance(logistic_ROC_prediction,"auc") 
logistic_auc_testing <- as.numeric(auc.tmp@y.values) 
logistic_auc_testing
#Lift chart
plotLift(logistic_probabilities, testing$Churn_numeric, cumulative = TRUE, n.buckets = 10) 

TP = 509.61
TN = -11.39
#predict by verify data and calculate profit
logistic_probabilities_verify <- predict(model_logistic_stepwiseAIC,newdata=verify,type="response")
verify_pridct<-rep("1",800)
verify_pridct[logistic_probabilities_verify< threshold]="0"
data_predict <- data.frame(verify$Churn_numeric,as.numeric(verify_pridct))
data_predict$profit <- ifelse(data_predict$verify.Churn_numeric == 1 & data_predict$as.numeric.verify_pridct. == 1, TP, 
                    ifelse(data_predict$verify.Churn_numeric==0 & data_predict$as.numeric.verify_pridct.==1, TN,0))
totalprofit <- sum(data_predict$profit)
totalprofit
###
#2.2 CTREE AUC0.8294401 profit 80657.63####
###

training[sapply(training, is.character)] <- lapply(training[sapply(training, is.character)], as.factor)
training$Churn_numeric <- as.factor(training$Churn_numeric)
str(training)

testing[sapply(testing, is.character)] <- lapply(testing[sapply(testing, is.character)], as.factor)
testing$Churn_numeric <- as.factor(testing$Churn_numeric)
str(testing)

pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")
ctree_tree <- ctree(Churn_numeric ~., data = training) 
plot(ctree_tree, gp = gpar(fontsize = 8)) 

#Predict probabilities
ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") 
ctree_classification<-rep("1",1870)
ctree_classification[ctree_probabilities[,2]<threshold] = "0" 
ctree_classification<-as.factor(ctree_classification)
#Confusion matrix  
confusionMatrix(ctree_classification,testing$Churn_numeric,positive = "1")

#ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") 
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$Churn_numeric) 
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") 
plot(ctree_ROC_testing) 

#AUC-0.8294401
auc.tmp <- performance(ctree_pred_testing,"auc") 
ctree_auc_testing <- as.numeric(auc.tmp@y.values) 
ctree_auc_testing 
# Lift chart
plotLift(ctree_probabilities[,2],  testing$Churn_numeric, cumulative = TRUE, n.buckets = 10) 

#predict by verify data and calculate profit
ctree_probabilities_verify<-predict(ctree_tree,newdata=verify,type="prob") 
ctree_classification<-rep("1",800)
ctree_classification[ctree_probabilities_verify[,2]<threshold] = "0"

data_predict <- data.frame(verify$Churn_numeric,as.numeric(ctree_classification))
data_predict$profit <- ifelse(data_predict$verify.Churn_numeric == 1 & data_predict$as.numeric.ctree_classification. == 1, TP, 
                              ifelse(data_predict$verify.Churn_numeric==0 & data_predict$as.numeric.ctree_classification.==1, TN,0))
totalprofit <- sum(data_predict$profit)
totalprofit



###
#2.3 RPART AUC0.8055489 profit 44728.84####
###
CART_cp = rpart.control(cp = 0.0005) 
rpart_tree<-rpart(Churn_numeric~.,data=training, method="class", control=CART_cp)

#Prun the tree. Play with cp to see how the resultant tree changes
prunned_rpart_tree<-prune(rpart_tree, cp=  0.0026  ) 
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) 
plotcp(rpart_tree) 
printcp(rpart_tree)

#Confusion matrix 
rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") 
rpart_prediction_class <- as.factor(ifelse(rpart_prediction_class == 1,1,0))
confusionMatrix(rpart_prediction_class,testing$Churn_numeric,positive = "1") 

#ROC Curve
rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") 
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$Churn_numeric) 
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") 
plot(rpart_ROC_testing)

#AUC 0.8055489
auc.tmp <- performance(rpart_pred_testing,"auc") 
rpart_auc_testing <- as.numeric(auc.tmp@y.values) 
rpart_auc_testing
# Lift chart
plotLift(rpart_prediction_class,  testing$Churn_numeric, cumulative = TRUE, n.buckets = 10) 


#predict by verify data and calculate profit
rpart_prediction_class_verify<-predict(prunned_rpart_tree,newdata=verify, type="class") 
rpart_prediction_class_verify <- as.numeric(ifelse(rpart_prediction_class_verify == 1,1,0))

data_predict <- data.frame(verify$Churn_numeric,rpart_prediction_class_verify)
data_predict$profit <- ifelse(data_predict$verify.Churn_numeric == 1 & data_predict$rpart_prediction_class_verify == 1, TP, 
                              ifelse(data_predict$verify.Churn_numeric==0 & data_predict$rpart_prediction_class_verify==1, TN,0))
totalprofit <- sum(data_predict$profit)
totalprofit




###
#2.4 Random Forest AUC0.7593234, profit 42929.59####
###
pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages
model_forest <- randomForest(Churn_numeric~., data=training, 
                             type="classification",
                             importance=TRUE,
                             ntree = 100,           # hyperparameter: number of trees in the forest
                             mtry = 48,             # hyperparameter: number of random columns to grow each tree
                             nodesize = 10,         # hyperparameter: min number of datapoints on the leaf of each tree
                             maxnodes = 10,         # hyperparameter: maximum number of leafs of a tree
                             cutoff = c(0.2, 0.8)   # hyperparameter: how the voting works; (0.5, 0.5) means majority vote
) 
plot(model_forest)  # plots error as a function of number of trees in the forest; use print(model_forest) to print the values on the plot
print(model_forest) 
# plots variable importances
varImpPlot(model_forest) 
importance(model_forest)

#Prediction
forest_probabilities<-predict(model_forest,newdata=testing,type="prob") 
forest_classification<-rep("1",1870)
forest_classification[forest_probabilities[,2]<threshold]="0" 
forest_classification<-as.factor(forest_classification)
#Confusion matrix 
confusionMatrix(forest_classification,testing$Churn_numeric, positive="1") 
#ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$Churn_numeric) 
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") 
plot(forest_ROC) 
#AUC 0.7593234
AUC.tmp <- performance(forest_ROC_prediction,"auc") 
forest_AUC <- as.numeric(AUC.tmp@y.values)
forest_AUC
#Lift chart
plotLift(forest_probabilities[,2],  testing$Churn_numeric, cumulative = TRUE, n.buckets = 10)

#predict by verify data and calculate profit
forest_probabilities_verify<-predict(model_forest,newdata=verify,type="prob") 
forest_classification<-rep("1",800)
forest_classification[forest_probabilities_verify[,2]<threshold]="0" 



data_predict <- data.frame(verify$Churn_numeric,as.numeric(forest_classification))
data_predict$profit <- ifelse(data_predict$verify.Churn_numeric == 1 & data_predict$as.numeric.forest_classification. == 1, TP, 
                              ifelse(data_predict$verify.Churn_numeric==0 & data_predict$as.numeric.forest_classification.==1, TN,0))
totalprofit <- sum(data_predict$profit)
totalprofit






###
#2.5 XGboost AUC0.8215029 profit70963.65####
###
pacman::p_load("caret","ROCR","lift","xgboost") 
STCdata_A_matrix <- model.matrix(Churn_numeric ~ . , data = test_train)[,-1]

x_train <- STCdata_A_matrix[ -testIndex,]
x_test <- STCdata_A_matrix[ testIndex,]


y_train <-training$Churn_numeric
y_test <-testing$Churn_numeric

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response") 
#Confusion matrix 
confusionMatrix(as.factor(ifelse(XGboost_prediction>threshold,1,0)),y_test,positive="1") 
#ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) 
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") 
plot(XGboost_ROC_testing) 
#AUC 0.8215029
auc.tmp <- performance(XGboost_ROC_prediction,"auc") 
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) 
XGboost_auc_testing 
#### Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


#predict by verify data and calculate profit
verify_matrix <- model.matrix(Churn_numeric ~ . , data = verify)[,-1]

XGboost_prediction_verify<-predict(model_XGboost,newdata=verify_matrix, type="response") 
xg <- ifelse(XGboost_prediction_verify>threshold,1,0)


data_predict <- data.frame(verify$Churn_numeric,xg)
data_predict$profit <- ifelse(data_predict$verify.Churn_numeric == 1 & data_predict$xg == 1, TP, 
                              ifelse(data_predict$verify.Churn_numeric==0 & data_predict$xg==1, TN,0))

totalprofit <- sum(data_predict$profit)
totalprofit



###
#2.6 SVM AUC0.1725723 profit 79631.79####
###try different kernels
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071")
model_svm <- svm(Churn_numeric ~., data=training, probability=TRUE)
#model_svm <- svm(Churn_numeric ~., data=training, probability=TRUE, kernel = 'sigmoid')#AUC 16%
#model_svm <- svm(Churn_numeric ~., data=training, probability=TRUE, kernel = 'linear') #AUC 17%
summary(model_svm)

svm_probabilities<-attr(predict(model_svm,newdata=testing, probability=TRUE), "prob")
svm_prediction<-svm_probabilities[,1]

svm_classification<-rep("1",1870)
svm_classification[svm_prediction<threshold]="0" 
svm_classification<-as.factor(svm_classification)
confusionMatrix(svm_classification,testing$Churn_numeric,positive = "1")

####ROC Curve
svm_ROC_prediction <- prediction(svm_prediction, testing$Churn_numeric) 
svm_ROC_testing <- performance(svm_ROC_prediction,"tpr","fpr") 
plot(svm_ROC_testing) #Plot ROC curve

####AUC 0.1725723
auc.tmp <- performance(svm_ROC_prediction,"auc") 
svm_auc_testing <- as.numeric(auc.tmp@y.values) 
svm_auc_testing 

#### Lift chart
plotLift(svm_prediction, testing$Churn_numeric, cumulative = TRUE, n.buckets = 10) # Plot Lift chart

#predict by verify data and calculate profit
svm_probabilities_verify<-attr(predict(model_svm,newdata=verify, probability=TRUE), "prob")
svm_prediction<-svm_probabilities_verify[,1]

svm_classification<-rep("1",800)
svm_classification[svm_prediction<threshold]="0" 

data_predict <- data.frame(verify$Churn_numeric,as.numeric(svm_classification))
data_predict$profit <- ifelse(data_predict$verify.Churn_numeric == 1 & data_predict$as.numeric.svm_classification. == 1, TP, 
                              ifelse(data_predict$verify.Churn_numeric==0 & data_predict$as.numeric.svm_classification.==1, TN,0))
totalprofit <- sum(data_predict$profit)
totalprofit


