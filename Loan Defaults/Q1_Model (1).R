#Part One - Prepare data####
#1.1 Load data####
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071") #Check, and if needed install the necessary packages
Packages <- c('tidyverse', 'mice', 'readxl', 'MASS','caret','ggplot2', 'tidyr', 'lubridate','lattice', 'car','estimatr')
lapply(Packages, library, character.only = TRUE)
rm(Packages)
STCdata_A <- read_excel(file.choose()) 
str(STCdata_A) 
summary(STCdata_A)

#remove ID
STCdata_A$ID <- NULL

#no missing value
md.pattern(STCdata_A)

# check for rare categories
table(STCdata_A$SEX)
table(STCdata_A$EDUCATION) #education 0,4,5,6 to others
table(STCdata_A$MARRIAGE) #marriage 0,3 to others
table(STCdata_A$AGE) #group age over 65+
table(STCdata_A$PAY_1)#group >=3 
table(STCdata_A$PAY_2)
table(STCdata_A$PAY_3)
table(STCdata_A$PAY_4)
table(STCdata_A$PAY_5)
table(STCdata_A$PAY_6)
table(STCdata_A$default_0)

#1.2 Feature engineering####
STCdata_A$EDUCATION[which(STCdata_A$EDUCATION %in% c('0','4','5','6'))] <- 'EDUCATION_Other'
table(STCdata_A$EDUCATION) 

STCdata_A$MARRIAGE[which(STCdata_A$MARRIAGE %in% c('0','3'))] <- 'MARRIAGE_Other'
table(STCdata_A$MARRIAGE) 

STCdata_A$PAY_total <- STCdata_A$PAY_6 + STCdata_A$PAY_5+ STCdata_A$PAY_4+ STCdata_A$PAY_3+ STCdata_A$PAY_2+ STCdata_A$PAY_1

table(STCdata_A$PAY_6)

STCdata_A$LIMITGT490K <- ifelse(STCdata_A$LIMIT_BAL >= 490000, 1,0)
table(STCdata_A$LIMITGT490K)

STCdata_A$AGEGT65 <- ifelse(STCdata_A$AGE >= 65, 1,0)
table(STCdata_A$AGEGT65)

#new vars - diff bill vs limit
STCdata_A$difd_limit_bill_1 <- STCdata_A$LIMIT_BAL - STCdata_A$BILL_AMT1
STCdata_A$difd_limit_bill_2 <- STCdata_A$LIMIT_BAL - STCdata_A$BILL_AMT2
STCdata_A$difd_limit_bill_3 <- STCdata_A$LIMIT_BAL - STCdata_A$BILL_AMT3
STCdata_A$difd_limit_bill_4 <- STCdata_A$LIMIT_BAL - STCdata_A$BILL_AMT4
STCdata_A$difd_limit_bill_5 <- STCdata_A$LIMIT_BAL - STCdata_A$BILL_AMT5
STCdata_A$difd_limit_bill_6 <- STCdata_A$LIMIT_BAL - STCdata_A$BILL_AMT6

#new vars - diff between bill amount
STCdata_A$diffbill_12 <- STCdata_A$BILL_AMT1-STCdata_A$BILL_AMT2
STCdata_A$diffbill_23 <- STCdata_A$BILL_AMT2-STCdata_A$BILL_AMT3
STCdata_A$diffbill_34 <- STCdata_A$BILL_AMT3-STCdata_A$BILL_AMT4
STCdata_A$diffbill_45 <- STCdata_A$BILL_AMT4-STCdata_A$BILL_AMT5
STCdata_A$diffbill_56 <- STCdata_A$BILL_AMT5-STCdata_A$BILL_AMT6

#new vars - scaling the bill amount 
STCdata_A$scal_billamount_1 <- STCdata_A$BILL_AMT1/STCdata_A$LIMIT_BAL
STCdata_A$scal_billamount_2 <- STCdata_A$BILL_AMT2/STCdata_A$LIMIT_BAL
STCdata_A$scal_billamount_3 <- STCdata_A$BILL_AMT3/STCdata_A$LIMIT_BAL
STCdata_A$scal_billamount_4 <- STCdata_A$BILL_AMT4/STCdata_A$LIMIT_BAL
STCdata_A$scal_billamount_5 <- STCdata_A$BILL_AMT5/STCdata_A$LIMIT_BAL
STCdata_A$scal_billamount_6 <- STCdata_A$BILL_AMT6/STCdata_A$LIMIT_BAL

#new vars - scaling the payment amount (pay and bill)
STCdata_A$scal_payment_1 <- ifelse(STCdata_A$BILL_AMT1 != 0,STCdata_A$PAY_1/STCdata_A$BILL_AMT1,0) 
STCdata_A$scal_payment_2 <- ifelse(STCdata_A$BILL_AMT2 != 0,STCdata_A$PAY_2/STCdata_A$BILL_AMT2,0) 
STCdata_A$scal_payment_3 <- ifelse(STCdata_A$BILL_AMT3 != 0,STCdata_A$PAY_3/STCdata_A$BILL_AMT3,0) 
STCdata_A$scal_payment_4 <- ifelse(STCdata_A$BILL_AMT4 != 0,STCdata_A$PAY_4/STCdata_A$BILL_AMT4,0) 
STCdata_A$scal_payment_5 <- ifelse(STCdata_A$BILL_AMT5 != 0,STCdata_A$PAY_5/STCdata_A$BILL_AMT5,0) 
STCdata_A$scal_payment_6 <- ifelse(STCdata_A$BILL_AMT6 != 0,STCdata_A$PAY_6/STCdata_A$BILL_AMT6,0) 

#new vars - overall payment percentages
STCdata_A$spayment_percent <- 
  ifelse((STCdata_A$BILL_AMT1+STCdata_A$BILL_AMT2+STCdata_A$BILL_AMT3+STCdata_A$BILL_AMT4+STCdata_A$BILL_AMT5+STCdata_A$BILL_AMT6) != 0,
         (STCdata_A$PAY_1+STCdata_A$PAY_2+STCdata_A$PAY_3+STCdata_A$PAY_4+STCdata_A$PAY_5+STCdata_A$PAY_6)/(STCdata_A$BILL_AMT1+STCdata_A$BILL_AMT2+STCdata_A$BILL_AMT3+STCdata_A$BILL_AMT4+STCdata_A$BILL_AMT5+STCdata_A$BILL_AMT6),0)

STCdata_A$PAY_1[which(STCdata_A$PAY_1 >= 3)] <- '>=3'
table(STCdata_A$PAY_1)

STCdata_A$PAY_2[which(STCdata_A$PAY_2 >= 3)] <- '>=3'
table(STCdata_A$PAY_2)

STCdata_A$PAY_3[which(STCdata_A$PAY_3 >= 3)] <- '>=3'
table(STCdata_A$PAY_3)

STCdata_A$PAY_4[which(STCdata_A$PAY_4 >= 3)] <- '>=3'
table(STCdata_A$PAY_4)

STCdata_A$PAY_5[which(STCdata_A$PAY_5 >= 3)] <- '>=3'
table(STCdata_A$PAY_5)

STCdata_A$PAY_6[which(STCdata_A$PAY_6 >= 3)] <- '>=3'

str(STCdata_A)

# Fixing incorrectly classified data types
STCdata_A$SEX <- as.factor(STCdata_A$SEX)
STCdata_A$EDUCATION <- as.factor(STCdata_A$EDUCATION)
STCdata_A$MARRIAGE <- as.factor(STCdata_A$MARRIAGE)
STCdata_A$PAY_1 <- as.factor(STCdata_A$PAY_1)
STCdata_A$PAY_2 <- as.factor(STCdata_A$PAY_2)
STCdata_A$PAY_3 <- as.factor(STCdata_A$PAY_3)
STCdata_A$PAY_4 <- as.factor(STCdata_A$PAY_4)
STCdata_A$PAY_5 <- as.factor(STCdata_A$PAY_5)
STCdata_A$PAY_6 <- as.factor(STCdata_A$PAY_6)
STCdata_A$default_0 <- as.factor(STCdata_A$default_0)
STCdata_A$LIMITGT490K <- as.factor(STCdata_A$LIMITGT490K)
STCdata_A$AGEGT65 <- as.factor(STCdata_A$AGEGT65)

str(STCdata_A) 
summary(STCdata_A)

#missing value
md.pattern(STCdata_A)

sum(is.na(STCdata_A$PAY_total))
sum(is.na(STCdata_A$LIMITGT490K))
sum(is.na(STCdata_A$AGEGT65))
sum(is.na(STCdata_A$difd_limit_bill_1))
sum(is.na(STCdata_A$difd_limit_bill_2))
sum(is.na(STCdata_A$difd_limit_bill_3))
sum(is.na(STCdata_A$difd_limit_bill_4))
sum(is.na(STCdata_A$difd_limit_bill_5))
sum(is.na(STCdata_A$difd_limit_bill_6))
sum(is.na(STCdata_A$diffbill_12))
sum(is.na(STCdata_A$diffbill_23))
sum(is.na(STCdata_A$diffbill_34))
sum(is.na(STCdata_A$diffbill_45))
sum(is.na(STCdata_A$diffbill_56))
sum(is.na(STCdata_A$scal_billamount_1))
sum(is.na(STCdata_A$scal_billamount_2))
sum(is.na(STCdata_A$scal_billamount_3))
sum(is.na(STCdata_A$scal_billamount_4))
sum(is.na(STCdata_A$scal_billamount_5))
sum(is.na(STCdata_A$scal_billamount_6))

#1.3 Missing values####
STCdata_A$scal_payment_1[which(is.na(STCdata_A$scal_payment_1))] <- 0
sum(is.na(STCdata_A$scal_payment_1)) 

STCdata_A$scal_payment_2[which(is.na(STCdata_A$scal_payment_2))] <- 0
sum(is.na(STCdata_A$scal_payment_2))

STCdata_A$scal_payment_3[which(is.na(STCdata_A$scal_payment_3))] <- 0
sum(is.na(STCdata_A$scal_payment_3))

STCdata_A$scal_payment_4[which(is.na(STCdata_A$scal_payment_4))] <- 0
sum(is.na(STCdata_A$scal_payment_4))

STCdata_A$scal_payment_5[which(is.na(STCdata_A$scal_payment_5))] <- 0
sum(is.na(STCdata_A$scal_payment_5))

STCdata_A$scal_payment_6[which(is.na(STCdata_A$scal_payment_6))] <- 0
sum(is.na(STCdata_A$scal_payment_6))

STCdata_A$spayment_percent[which(is.na(STCdata_A$spayment_percent))] <- 0
sum(is.na(STCdata_A$spayment_percent))
md.pattern(STCdata_A)
str(STCdata_A)
summary(STCdata_A)

#1.4 Infinite values####
sum(is.infinite(STCdata_A$PAY_total))
sum(is.infinite(STCdata_A$LIMITGT490K))
sum(is.infinite(STCdata_A$AGEGT65))
sum(is.infinite(STCdata_A$difd_limit_bill_1))
sum(is.infinite(STCdata_A$difd_limit_bill_2))
sum(is.infinite(STCdata_A$difd_limit_bill_3))
sum(is.infinite(STCdata_A$difd_limit_bill_4))
sum(is.infinite(STCdata_A$difd_limit_bill_5))
sum(is.infinite(STCdata_A$difd_limit_bill_6))
sum(is.infinite(STCdata_A$diffbill_12))
sum(is.infinite(STCdata_A$diffbill_23))
sum(is.infinite(STCdata_A$diffbill_34))
sum(is.infinite(STCdata_A$diffbill_45))
sum(is.infinite(STCdata_A$diffbill_56))
sum(is.infinite(STCdata_A$scal_billamount_1))
sum(is.infinite(STCdata_A$scal_billamount_2))
sum(is.infinite(STCdata_A$scal_billamount_3))
sum(is.infinite(STCdata_A$scal_billamount_4))
sum(is.infinite(STCdata_A$scal_billamount_5))
sum(is.infinite(STCdata_A$scal_billamount_6))
sum(is.infinite(STCdata_A$scal_payment_1))
sum(is.infinite(STCdata_A$scal_payment_2))
sum(is.infinite(STCdata_A$scal_payment_3))
sum(is.infinite(STCdata_A$scal_payment_4))
sum(is.infinite(STCdata_A$scal_payment_5))
sum(is.infinite(STCdata_A$scal_payment_6))
sum(is.infinite(STCdata_A$spayment_percent))

str(STCdata_A)

#1.5 Split testing and training####
set.seed(1111) #set a random number generation seed to ensure that the split is the same everytime
inTrain <- createDataPartition(y = STCdata_A$default_0,
                               p = 15999/24000, list = FALSE)
training <- STCdata_A[ inTrain,]
testing <- STCdata_A[ -inTrain,]

#Part Two - Models####
###
#2.1 Logistic regression + AIC####
###
model_logistic <- glm(default_0 ~ . -ID , data=training, family="binomial"(link="logit")) 
summary(model_logistic) 
model_logistic_stepwiseAIC<-stepAIC(model_logistic,direction = c("both"),trace = 1) 
summary(model_logistic_stepwiseAIC) 

par(mfrow=c(1,4))
plot(model_logistic_stepwiseAIC) #Error plots: similar nature to lm plots
par(mfrow=c(1,1))

#predict
logistic_probabilities<-predict(model_logistic_stepwiseAIC,newdata=testing,type="response")
logistic_classification<-rep("1",8000)
logistic_classification[logistic_probabilities<0.2]="0" 
logistic_classification<-as.factor(logistic_classification)
#Confusion matrix  
confusionMatrix(logistic_classification,testing$default_0,positive = "1") 
#ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, testing$default_0)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") 
plot(logistic_ROC) #Plot ROC curve
#AUC
auc.tmp <- performance(logistic_ROC_prediction,"auc") 
logistic_auc_testing <- as.numeric(auc.tmp@y.values) 
logistic_auc_testing
#Lift chart
plotLift(logistic_probabilities, testing$default_0, cumulative = TRUE, n.buckets = 10) 


###
#2.2 CTREE####
###
pacman::p_load("caret","partykit","ROCR","lift","rpart","e1071")
ctree_tree <- ctree(default_0 ~., data = training) 
plot(ctree_tree, gp = gpar(fontsize = 8)) 
#Predict probabilities
ctree_probabilities<-predict(ctree_tree,newdata=testing,type="prob") 
ctree_classification<-rep("1",8000)
ctree_classification[ctree_probabilities[,2]<0.2]="0" 
ctree_classification<-as.factor(ctree_classification)
#Confusion matrix  
confusionMatrix(ctree_classification,testing$default_0,positive = "1")
#ROC Curve
ctree_probabilities_testing <-predict(ctree_tree,newdata=testing,type = "prob") 
ctree_pred_testing <- prediction(ctree_probabilities_testing[,2], testing$default_0) 
ctree_ROC_testing <- performance(ctree_pred_testing,"tpr","fpr") 
plot(ctree_ROC_testing) 
#AUC
auc.tmp <- performance(ctree_pred_testing,"auc") 
ctree_auc_testing <- as.numeric(auc.tmp@y.values) 
ctree_auc_testing 
# Lift chart
plotLift(ctree_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10) 


###
#2.3 RPART####
###
str(training)
df = subset(training, select = -c(ID) )
CART_cp = rpart.control(cp = 0.0005) 
rpart_tree<-rpart(default_0~.,data=df, method="class", control=CART_cp)

#Prun the tree. Play with cp to see how the resultant tree changes
prunned_rpart_tree<-prune(rpart_tree, cp=  0.0013  ) 
plot(as.party(prunned_rpart_tree), type = "extended",gp = gpar(fontsize = 7)) 
plotcp(rpart_tree) 
printcp(rpart_tree)
#Confusion matrix 
rpart_prediction_class<-predict(prunned_rpart_tree,newdata=testing, type="class") 
confusionMatrix(rpart_prediction_class,testing$default_0,positive = "1") 
#ROC Curve
rpart_probabilities_testing <-predict(prunned_rpart_tree,newdata=testing,type = "prob") 
rpart_pred_testing <- prediction(rpart_probabilities_testing[,2], testing$default_0) 
rpart_ROC_testing <- performance(rpart_pred_testing,"tpr","fpr") 
plot(rpart_ROC_testing) #Plot ROC curve
#AUC
auc.tmp <- performance(rpart_pred_testing,"auc") 
rpart_auc_testing <- as.numeric(auc.tmp@y.values) 
rpart_auc_testing #Display AUC value
# Lift chart
plotLift(rpart_prediction_class,  testing$default_0, cumulative = TRUE, n.buckets = 10) 


###
#2.4 Random Forest####
###
pacman::p_load("caret","ROCR","lift","randomForest") #Check, and if needed install the necessary packages
model_forest <- randomForest(default_0~., data=training, 
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
forest_classification<-rep("1",8000)
forest_classification[forest_probabilities[,2]<0.2]="0" 
forest_classification<-as.factor(forest_classification)
#Confusion matrix 
confusionMatrix(forest_classification,testing$default_0, positive="1") 
#ROC Curve
forest_ROC_prediction <- prediction(forest_probabilities[,2], testing$default_0) 
forest_ROC <- performance(forest_ROC_prediction,"tpr","fpr") 
plot(forest_ROC) 
#AUC
AUC.tmp <- performance(forest_ROC_prediction,"auc") 
forest_AUC <- as.numeric(AUC.tmp@y.values)
forest_AUC
#Lift chart
plotLift(forest_probabilities[,2],  testing$default_0, cumulative = TRUE, n.buckets = 10)


###
#2.5 Gradient Boosting Machines (XGboost)####
###
pacman::p_load("caret","ROCR","lift","xgboost") 
STCdata_A_matrix <- model.matrix(default_0 ~ . , data = STCdata_A)[,-1]

x_train <- STCdata_A_matrix[ inTrain,]
x_test <- STCdata_A_matrix[ -inTrain,]
y_train <-training$default_0
y_test <-testing$default_0

model_XGboost<-xgboost(data = data.matrix(x_train), 
                       label = as.numeric(as.character(y_train)), 
                       eta = 0.1,       # hyperparameter: learning rate 
                       max_depth = 20,  # hyperparameter: size of a tree in each boosting iteration
                       nround=50,       # hyperparameter: number of boosting iterations  
                       objective = "binary:logistic"
)

XGboost_prediction<-predict(model_XGboost,newdata=x_test, type="response") 
#Confusion matrix 
confusionMatrix(as.factor(ifelse(XGboost_prediction>0.2,1,0)),y_test,positive="1") 
#ROC Curve
XGboost_ROC_prediction <- prediction(XGboost_prediction, y_test) 
XGboost_ROC_testing <- performance(XGboost_ROC_prediction,"tpr","fpr") 
plot(XGboost_ROC_testing) 
#AUC
auc.tmp <- performance(XGboost_ROC_prediction,"auc") 
XGboost_auc_testing <- as.numeric(auc.tmp@y.values) 
XGboost_auc_testing 
#### Lift chart
plotLift(XGboost_prediction, y_test, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


###
#2.6 Support Vector Machines####
###
pacman::p_load("caret","ROCR","lift","glmnet","MASS","e1071")
model_svm <- svm(default_0 ~., data=training, probability=TRUE)
summary(model_svm)
svm_probabilities<-attr(predict(model_svm,newdata=testing, probability=TRUE), "prob")
svm_prediction<-svm_probabilities[,1]

svm_classification<-rep("1",8000)
svm_classification[svm_prediction<0.2]="0" 
svm_classification<-as.factor(svm_classification)
confusionMatrix(svm_classification,testing$default_0,positive = "1")

#ROC Curve
svm_ROC_prediction <- prediction(svm_prediction, testing$default_0)
svm_ROC_testing <- performance(svm_ROC_prediction,"tpr","fpr") 
plot(svm_ROC_testing)
#AUC
auc.tmp <- performance(svm_ROC_prediction,"auc") 
svm_auc_testing <- as.numeric(auc.tmp@y.values) 
svm_auc_testing 
#Lift chart
plotLift(svm_prediction, testing$default_0, cumulative = TRUE, n.buckets = 10) 

###
#Part Three - Predict ####
#3.1 Prepare data####
pred<- read_excel(file.choose()) 
str(pred) 
summary(pred)

#missing value
md.pattern(pred)

# check for rare categories
table(pred$SEX)
table(pred$EDUCATION) #education 0,4,5,6 to others
table(pred$MARRIAGE) #marriage 0,3 to others
table(pred$AGE) #group age over 65+
table(pred$PAY_1)#group >=3 
table(pred$PAY_2)
table(pred$PAY_3)
table(pred$PAY_4)
table(pred$PAY_5)
table(pred$PAY_6)

#Feature engineering
pred$EDUCATION[which(pred$EDUCATION %in% c('0','4','5','6'))] <- 'EDUCATION_Other'
table(pred$EDUCATION) 
pred$MARRIAGE[which(pred$MARRIAGE %in% c('0','3'))] <- 'MARRIAGE_Other'
table(pred$MARRIAGE) 
pred$PAY_total <- pred$PAY_6 + pred$PAY_5+ pred$PAY_4+ pred$PAY_3+ pred$PAY_2+ pred$PAY_1
table(pred$PAY_6)
pred$LIMITGT490K <- ifelse(pred$LIMIT_BAL >= 490000, 1,0)
table(pred$LIMITGT490K)
pred$AGEGT65 <- ifelse(pred$AGE >= 65, 1,0)
table(pred$AGEGT65)

#new vars - diff bill vs limit
pred$difd_limit_bill_1 <- pred$LIMIT_BAL - pred$BILL_AMT1
pred$difd_limit_bill_2 <- pred$LIMIT_BAL - pred$BILL_AMT2
pred$difd_limit_bill_3 <- pred$LIMIT_BAL - pred$BILL_AMT3
pred$difd_limit_bill_4 <- pred$LIMIT_BAL - pred$BILL_AMT4
pred$difd_limit_bill_5 <- pred$LIMIT_BAL - pred$BILL_AMT5
pred$difd_limit_bill_6 <- pred$LIMIT_BAL - pred$BILL_AMT6

#new vars - diff between bill amount
pred$diffbill_12 <- pred$BILL_AMT1-pred$BILL_AMT2
pred$diffbill_23 <- pred$BILL_AMT2-pred$BILL_AMT3
pred$diffbill_34 <- pred$BILL_AMT3-pred$BILL_AMT4
pred$diffbill_45 <- pred$BILL_AMT4-pred$BILL_AMT5
pred$diffbill_56 <- pred$BILL_AMT5-pred$BILL_AMT6

#new vars - scaling the bill amount 
pred$scal_billamount_1 <- pred$BILL_AMT1/pred$LIMIT_BAL
pred$scal_billamount_2 <- pred$BILL_AMT2/pred$LIMIT_BAL
pred$scal_billamount_3 <- pred$BILL_AMT3/pred$LIMIT_BAL
pred$scal_billamount_4 <- pred$BILL_AMT4/pred$LIMIT_BAL
pred$scal_billamount_5 <- pred$BILL_AMT5/pred$LIMIT_BAL
pred$scal_billamount_6 <- pred$BILL_AMT6/pred$LIMIT_BAL

#new vars - scaling the payment amount (pay and bill)
pred$scal_payment_1 <- ifelse(pred$BILL_AMT1 != 0,pred$PAY_1/pred$BILL_AMT1,0) 
pred$scal_payment_2 <- ifelse(pred$BILL_AMT2 != 0,pred$PAY_2/pred$BILL_AMT2,0) 
pred$scal_payment_3 <- ifelse(pred$BILL_AMT3 != 0,pred$PAY_3/pred$BILL_AMT3,0) 
pred$scal_payment_4 <- ifelse(pred$BILL_AMT4 != 0,pred$PAY_4/pred$BILL_AMT4,0) 
pred$scal_payment_5 <- ifelse(pred$BILL_AMT5 != 0,pred$PAY_5/pred$BILL_AMT5,0) 
pred$scal_payment_6 <- ifelse(pred$BILL_AMT6 != 0,pred$PAY_6/pred$BILL_AMT6,0) 

#new vars - overall payment percentages
pred$spayment_percent <- 
  ifelse((pred$BILL_AMT1+pred$BILL_AMT2+pred$BILL_AMT3+pred$BILL_AMT4+pred$BILL_AMT5+pred$BILL_AMT6) != 0,
         (pred$PAY_1+pred$PAY_2+pred$PAY_3+pred$PAY_4+pred$PAY_5+pred$PAY_6)/(pred$BILL_AMT1+pred$BILL_AMT2+pred$BILL_AMT3+pred$BILL_AMT4+pred$BILL_AMT5+pred$BILL_AMT6),0)

pred$PAY_1[which(pred$PAY_1 >= 3)] <- '>=3'
table(pred$PAY_1)

pred$PAY_2[which(pred$PAY_2 >= 3)] <- '>=3'
table(pred$PAY_2)

pred$PAY_3[which(pred$PAY_3 >= 3)] <- '>=3'
table(pred$PAY_3)

pred$PAY_4[which(pred$PAY_4 >= 3)] <- '>=3'
table(pred$PAY_4)

pred$PAY_5[which(pred$PAY_5 >= 3)] <- '>=3'
table(pred$PAY_5)

pred$PAY_6[which(pred$PAY_6 >= 3)] <- '>=3'

str(pred)

# Fixing incorrectly classified data types
pred$SEX <- as.factor(pred$SEX)
pred$EDUCATION <- as.factor(pred$EDUCATION)
pred$MARRIAGE <- as.factor(pred$MARRIAGE)
pred$PAY_1 <- as.factor(pred$PAY_1)
pred$PAY_2 <- as.factor(pred$PAY_2)
pred$PAY_3 <- as.factor(pred$PAY_3)
pred$PAY_4 <- as.factor(pred$PAY_4)
pred$PAY_5 <- as.factor(pred$PAY_5)
pred$PAY_6 <- as.factor(pred$PAY_6)
pred$LIMITGT490K <- as.factor(pred$LIMITGT490K)
pred$AGEGT65 <- as.factor(pred$AGEGT65)

str(pred) 
summary(pred)

#missing value
md.pattern(pred)
sum(is.na(pred$PAY_total))
sum(is.na(pred$LIMITGT490K))
sum(is.na(pred$AGEGT65))
sum(is.na(pred$difd_limit_bill_1))
sum(is.na(pred$difd_limit_bill_2))
sum(is.na(pred$difd_limit_bill_3))
sum(is.na(pred$difd_limit_bill_4))
sum(is.na(pred$difd_limit_bill_5))
sum(is.na(pred$difd_limit_bill_6))
sum(is.na(pred$diffbill_12))
sum(is.na(pred$diffbill_23))
sum(is.na(pred$diffbill_34))
sum(is.na(pred$diffbill_45))
sum(is.na(pred$diffbill_56))
sum(is.na(pred$scal_billamount_1))
sum(is.na(pred$scal_billamount_2))
sum(is.na(pred$scal_billamount_3))
sum(is.na(pred$scal_billamount_4))
sum(is.na(pred$scal_billamount_5))
sum(is.na(pred$scal_billamount_6))

#Missing values
pred$scal_payment_1[which(is.na(pred$scal_payment_1))] <- 0
sum(is.na(pred$scal_payment_1)) 

pred$scal_payment_2[which(is.na(pred$scal_payment_2))] <- 0
sum(is.na(pred$scal_payment_2))

pred$scal_payment_3[which(is.na(pred$scal_payment_3))] <- 0
sum(is.na(pred$scal_payment_3))

pred$scal_payment_4[which(is.na(pred$scal_payment_4))] <- 0
sum(is.na(pred$scal_payment_4))

pred$scal_payment_5[which(is.na(pred$scal_payment_5))] <- 0
sum(is.na(pred$scal_payment_5))

pred$scal_payment_6[which(is.na(pred$scal_payment_6))] <- 0
sum(is.na(pred$scal_payment_6))

pred$spayment_percent[which(is.na(pred$spayment_percent))] <- 0
sum(is.na(pred$spayment_percent))
md.pattern(pred)
str(pred)
summary(pred)

#is.infinite()
sum(is.infinite(pred$PAY_total))
sum(is.infinite(pred$LIMITGT490K))
sum(is.infinite(pred$AGEGT65))
sum(is.infinite(pred$difd_limit_bill_1))
sum(is.infinite(pred$difd_limit_bill_2))
sum(is.infinite(pred$difd_limit_bill_3))
sum(is.infinite(pred$difd_limit_bill_4))
sum(is.infinite(pred$difd_limit_bill_5))
sum(is.infinite(pred$difd_limit_bill_6))
sum(is.infinite(pred$diffbill_12))
sum(is.infinite(pred$diffbill_23))
sum(is.infinite(pred$diffbill_34))
sum(is.infinite(pred$diffbill_45))
sum(is.infinite(pred$diffbill_56))
sum(is.infinite(pred$scal_billamount_1))
sum(is.infinite(pred$scal_billamount_2))
sum(is.infinite(pred$scal_billamount_3))
sum(is.infinite(pred$scal_billamount_4))
sum(is.infinite(pred$scal_billamount_5))
sum(is.infinite(pred$scal_billamount_6))
sum(is.infinite(pred$scal_payment_1))
sum(is.infinite(pred$scal_payment_2))
sum(is.infinite(pred$scal_payment_3))
sum(is.infinite(pred$scal_payment_4))
sum(is.infinite(pred$scal_payment_5))
sum(is.infinite(pred$scal_payment_6))
sum(is.infinite(pred$spayment_percent))

str(training)
summary(pred$PAY_4)
summary(training$PAY_4)
summary(pred$PAY_3)
summary(training$PAY_3)

summary(training)
#3.2 model####
ctree_probabilities<-predict(ctree_tree,newdata=pred,type="prob") 
#export default predict file
ctree_classification<-rep("1",1000)
ctree_classification[ctree_probabilities[,2]<0.2]="0" 
write.csv(ctree_classification, "predict_default.csv", row.names = FALSE)

#export issue or not predict file
ctree_classification_issue<-rep("0",1000)
ctree_classification_issue[ctree_probabilities[,2]<0.2]="1" 
write.csv(ctree_classification_issue, "predict_issue.csv", row.names = FALSE)

