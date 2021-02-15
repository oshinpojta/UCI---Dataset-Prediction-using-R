# Name : Oshin Pojta
# Roll.No. : EM017_REM017B60

library(class)
library(psycho)
library(tidyverse)
library(e1071)
library(rpart)
library(randomForest)
library(Matrix)
library(caTools)
library(C50)


dataset=read.csv("SomervilleHappinessSurvey2015.csv", fileEncoding="UTF-16LE")
table(dataset$D)
str(dataset)
dataset$D = as.factor(dataset$D)
dataset$D
str(dataset)
summary(dataset)
normalize=function(x)
{
  return((x-min(x))/(max(x)-min(x)))
}                                           
dataset_n=as.data.frame(lapply(dataset[2:7], normalize))
#dataset_n = scale(dataset_n)

set.seed(12323)   #random selection seed
split = sample.split(dataset$D, SplitRatio = 0.7)
dataset_train=subset(dataset_n,split == TRUE)      #training dataset
dataset_test=subset(dataset_n,split == FALSE)      #testing dataset

#View(dataset_train)
#View(dataset_test)

dataset_train_label=subset(dataset[,1],split == TRUE)
dataset_test_label=subset(dataset[,1],split == FALSE)

#View(dataset_train_label)
#View(dataset_test_label)

########  TRYING VARIOUS ALGORITMSS TO CHECK IF ANY FITS BETTER THAN OTHER. ########### 
########   IMPROVING MODEL PERFORMANCE SIMULTANEOUSLY FIND BEST POSSIBLE VALUES #######


# Random Forests used at trees - 6
classifier_RF = randomForest(x =dataset_train, y = dataset_train_label, ntree = 6)
y_predict_RF = predict(classifier_RF, newdata = dataset_test)
cm_RF = table(dataset_test_label,y_predict_RF)
cm_RF
df_RF = as.integer(cm_RF)
accuracy_RF = (((df_RF[1]+df_RF[2]+df_RF[3]+df_RF[4])-(df_RF[2]+df_RF[3]))/(df_RF[1]+df_RF[2]+df_RF[3]+df_RF[4]))*100
print(paste("Accuracy of RandomForest T-6 : ",accuracy_RF))

# Trying to improve model performance Random Forests used at trees - 10
classifier_RF = randomForest(x =dataset_train, y = dataset_train_label, ntree = 10)
y_predict_RF = predict(classifier_RF, newdata = dataset_test)
cm_RF = table(dataset_test_label,y_predict_RF)
cm_RF
df_RF = as.integer(cm_RF)
accuracy_RF = (((df_RF[1]+df_RF[2]+df_RF[3]+df_RF[4])-(df_RF[2]+df_RF[3]))/(df_RF[1]+df_RF[2]+df_RF[3]+df_RF[4]))*100
print(paste("Accuracy of RandomForest T-10 : ",accuracy_RF))



# Decision Trees by RPART
classifier_RPART = rpart(formula = D ~ ., data = dataset[1:100,])
y_predict_RPART = predict(classifier_RPART, newdata = dataset[101:142,], type = 'class')
cm_RPART = table(dataset[101:142,1],y_predict_RPART)
cm_RPART
df_RPART = as.integer(cm_RPART)
accuracy_RPART = (((df_RPART[1]+df_RPART[2]+df_RPART[3]+df_RPART[4])-(df_RPART[2]+df_RPART[3]))/(df_RPART[1]+df_RPART[2]+df_RPART[3]+df_RPART[4]))*100
print(paste("Accuracy of RPART trees : ",accuracy_RPART))



# Decision Trees by C50
classifier_C50 = C5.0(formula = D ~ ., data = dataset[1:100,])
y_predict_C50 = predict(classifier_C50, newdata = dataset[101:142,], type = 'class')
cm_C50= table(dataset[101:142,1],y_predict_C50)
cm_C50
df_C50 = as.integer(cm_C50)
accuracy_C50 = (((df_C50[1]+df_C50[2]+df_C50[3]+df_C50[4])-(df_C50[2]+df_C50[3]))/(df_C50[1]+df_C50[2]+df_C50[3]+df_C50[4]))*100
print(paste("Accuracy of C50 trees : ",accuracy_C50))



#Naive Bayes
classifier_NB = naiveBayes(x = dataset[1:100,2:7], y = dataset[1:100,1])
y_predict_NB = predict(classifier_NB, newdata = dataset[101:142,2:7])
cm_NB = table(dataset[101:142,1],y_predict_NB)
cm_NB
df_NB = as.integer(cm_NB)
accuracy_NB = (((df_NB[1]+df_NB[2]+df_NB[3]+df_NB[4])-(df_NB[2]+df_NB[3]))/(df_NB[1]+df_NB[2]+df_NB[3]+df_NB[4]))*100
print(paste("Accuracy of naiveBayes : ",accuracy_NB))



# SVM - linear
classifier_SVM = svm(formula = D ~ X1+X2+X5 , data = dataset[1:100,], type = 'C-classification', kernel='linear')
y_predict_SVM = predict(classifier_SVM, newdata = dataset[101:142,])
cm_SVM = table(dataset[101:142,1],y_predict_SVM)
cm_SVM
df_SVM = as.integer(cm_SVM)
accuracy_SVM = (((df_SVM[1]+df_SVM[2]+df_SVM[3]+df_SVM[4])-(df_SVM[2]+df_SVM[3]))/(df_SVM[1]+df_SVM[2]+df_SVM[3]+df_SVM[4]))*100
print(paste("Accuracy of SVM-linear : ",accuracy_SVM))

# Used multiple linearRegression backwardElimination to find less profitable columns and removing them from training set
# SVM - radial, TRYING TO IMPROVE MODEL PERFORMANCE: Eliminating less profitable columns
classifier_SVM = svm(formula = D ~ X1+X2+X5 , data = dataset[1:100,], type = 'C-classification', kernel='radial')
y_predict_SVM = predict(classifier_SVM, newdata = dataset[101:142,])
cm_SVM = table(dataset[101:142,1],y_predict_SVM)
cm_SVM
df_SVM = as.integer(cm_SVM)
accuracy_SVM = (((df_SVM[1]+df_SVM[2]+df_SVM[3]+df_SVM[4])-(df_SVM[2]+df_SVM[3]))/(df_SVM[1]+df_SVM[2]+df_SVM[3]+df_SVM[4]))*100
print(paste("Accuracy of SVM-radial : ",accuracy_SVM))



# KNN
dataset_pred=knn(train=dataset_train,test = dataset_test,dataset_train_label,k=21)
cm_KNN = table(dataset_pred,dataset_test_label)
df_KNN = as.integer(cm_KNN)
accuracy_KNN = (((df_KNN[1]+df_KNN[2]+df_KNN[3]+df_KNN[4])-(df_KNN[2]+df_KNN[3]))/(df_KNN[1]+df_KNN[2]+df_KNN[3]+df_KNN[4]))*100
print(paste("Accuracy of KNN k-21 : ",accuracy_KNN))

# Trying to improve the model performance using scale instead of normalize

dataset_n=as.data.frame(scale(dataset[-1]))

# Testing alternative values of k
dataset_pred=knn(train=dataset_train,test = dataset_test,dataset_train_label,k=15)
cm_KNN = table(dataset_pred,dataset_test_label)
df_KNN = as.integer(cm_KNN)
accuracy_KNN = (((df_KNN[1]+df_KNN[2]+df_KNN[3]+df_KNN[4])-(df_KNN[2]+df_KNN[3]))/(df_KNN[1]+df_KNN[2]+df_KNN[3]+df_KNN[4]))*100
print(paste("Accuracy of KNN k-15 and scaling : ",accuracy_KNN))

# Testing alternative values of k
dataset_pred=knn(train=dataset_train,test = dataset_test,dataset_train_label,k=30)

df_KNN = as.integer(table(dataset_pred,dataset_test_label))
accuracy_KNN = (((df_KNN[1]+df_KNN[2]+df_KNN[3]+df_KNN[4])-(df_KNN[2]+df_KNN[3]))/(df_KNN[1]+df_KNN[2]+df_KNN[3]+df_KNN[4]))*100
print(paste("Accuracy of KNN now k-30 : ",accuracy_KNN))



# SVM Radial with Multiple linear regression Back Elimination
print(paste("Hence the Winner is SVM - Radial, provides most accuracy out of all !!!"))
print(paste("SVM-Radial used with Back-Elimination of Multiple L-Regression Accuracy :: ", accuracy_SVM, "%"))

