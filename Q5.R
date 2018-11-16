library(e1071)
library(caret)
library(ROCR)

setwd("C:/Users/Malvika/Documents/Machine Learning/HW5")
train = read.csv("titanic_train.csv")
test = read.csv("titanic_test_labeled.csv")

#Remove irrelevant columns
train = train[, -c(1, 4, 9, 11)]
test = test[, - c(3, 8, 10)]

#Change test column names
colnames(test) = c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")

#Fill missing values of Age
  
train$Age[is.na(train$Age)] = mean(train$Age[!is.na(train$Age)])
train = train[-which(train$Embarked == ""),]
train$Embarked = factor(train$Embarked, levels=levels(test$Embarked))

test$Age[is.na(test$Age)] = mean(train$Age)
test$Fare[is.na(test$Fare)]= mean(train$Fare)

#Q5.1 Linear SVM

svm.fit = svm(Survived ~., data = train, kernel = 'linear', cost = 0.01)
summary(svm.fit)

#Number of Support Vectors = 565

cost = c(0.001, 0.1, 1, 5,10 )
set.seed(1)
for (i in cost){
  svmfit = svm(Survived~., data = train, kernel = "linear", cost = i)
  print(summary(svmfit))
}

#Number of Support Vectors for cost=0.001 is 680
#Number of Support Vectors for cost=0.1 is 422
#Number of Support Vectors for cost= 1 is 414
#Number of Support Vectors for cost= 5 is 413
#Number of Support Vectors for cost=10 is 415

#Q5.2 Tuning with cost parameter 
set.seed(1)
tune.out = tune(svm, Survived~., data = train, kernel = 'linear', ranges = list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
summary(tune.out)
bestmod = tune.out$best.model
summary(bestmod)  #Cost = 0.01 results in the lowest cross-validation error rate

#Q5.3 Radial SVM
set.seed(1)
radial.svm = tune(svm, Survived~., data = train, kernel = "radial", ranges = list(cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4)))
summary(radial.svm) #Best choices of parameters involves cost = 0.1 and gamma = 0.5

#Q5.4 Polynomial SVM
set.seed(1)
polynomial.svm = tune(svm, Survived~., data = train, kernel = "polynomial", ranges = list(cost=c(0.1, 1, 10, 100, 1000), degree=c(0.5, 1, 2, 3, 4)))
summary(polynomial.svm) #Best choices of parameters involves cost = 1 and degree = 2

#Q5.5 SVM Test Results

#Linear

cost = c(0.001, 0.1, 1, 5,10 )
accuracy.linear = c()
set.seed(2)
for (i in seq(5)){
  svmfit = svm(Survived~., data = train, kernel = "linear", cost = cost[i])
  pred.linear = predict(svmfit, test[, -1])
  pred.linear = as.vector(pred.linear)
  pred.linear = 1*(pred.linear>0.5)
  CM.linear = confusionMatrix(pred.linear, test$Survived)
  print(CM.linear$overall['Accuracy'])
  accuracy.linear[i] = CM.linear$overall['Accuracy']
}

#The cost values 0.1, 1, 5, 10 result in the same accuracy rate of 76% which is better
#than the cost value 0.001 which gives 64% accuracy rate 

#Radial
cost = c(0.001, 0.1, 1, 5,10 )
gamma = c(0.5, 1, 2, 3, 4)
best.gamma = c()
accuracy.radial = c()
set.seed(2)
for (i in seq(5)){
  max_acc = 0
  max_gamma = 0
  for(j in gamma){
  radial.svmfit = svm(Survived~., data = train, kernel = "radial", cost = cost[i], gamma = j)
  pred.radial = predict(radial.svmfit, test[, -1])
  pred.radial = as.vector(pred.radial)
  pred.radial = 1*(pred.radial>0.5)
  CM.radial = confusionMatrix(pred.radial, test$Survived)
  print(cost[i])
  print(j)
  print(CM.radial$overall['Accuracy'])
  if (CM.radial$overall['Accuracy'] > max_acc){
    max_acc = CM.radial$overall['Accuracy']
    max_gamma = j
  }
  accuracy.radial[i] = max_acc
  best.gamma[i] = max_gamma
  }
}
# Cost = 1 and Gamma = 0.5 results in the best accuracy rate of 77.51%

#Polynomial
cost = c(0.001, 0.1, 1, 5,10 )
degree = c(0.5, 1, 2, 3, 4)
best.degree = c()
accuracy.polynomial = c()
set.seed(2)
for (i in seq(5)){
  max.acc = 0
  max_degree = 0
  for(d in degree){
    polynomial.svmfit = svm(Survived~., data = train, kernel = "polynomial", cost = cost[i], degree = d)
    pred.polynomial = predict(polynomial.svmfit, test[, -1])
    pred.polynomial = as.vector(pred.polynomial)
    pred.polynomial= 1*(pred.polynomial>0.5)
    CM.polynomial = confusionMatrix(pred.polynomial, test$Survived)
    print(cost[i])
    print(d)
    print(CM.polynomial$overall['Accuracy'])
    if (CM.polynomial$overall['Accuracy'] > max.acc){
      max.acc = CM.polynomial$overall['Accuracy']
      max_degree = d
    }
    accuracy.polynomial[i] = max.acc
    best.degree[i] = max_degree
  }
}


#cost=1 degree=2 ; cost=5 degree=2 ; cost= 10 degree=2 all result in the best accuracy rate of 77.03%

#Q5.6 Plotting the results in a table

#Linear
#Table showing costs and their respective accuracy rates 
linear.results = data.frame(cost = cost, accuracy = accuracy.linear)
print(linear.results)

#Radial
radial.results = data.frame(cost = cost, gamma = best.gamma, accuracy = accuracy.radial)
print(radial.results)

#Polynomial
polynomial.results = data.frame(cost= cost, degree = best.degree, accuracy = accuracy.polynomial)
print(polynomial.results)