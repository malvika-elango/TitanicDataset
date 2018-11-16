library(ggplot2)
library(rpart)
library("party")

setwd("C:/Users/Malvika/Documents/Machine Learning/HW5")
train = read.csv("titanic_train.csv")
test = read.csv("titanic_test_labeled.csv")


#Remove irrelevant variables
train = train[, -c(1, 4, 9, 11)]
test = test[, - c(3, 8, 10)]

#Change test column names
colnames(test) = c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")

#Dealing with missing values
train$Age[is.na(train$Age)] = mean(train$Age[!is.na(train$Age)])
train = train[-which(train$Embarked == ""),]

train$Embarked = factor(train$Embarked, levels=levels(test$Embarked))

test$Age[is.na(test$Age)] = mean(train$Age)
test$Fare[is.na(test$Fare)]= mean(train$Fare)

alpha_fun <-function(epsilon){
  (1/2)*log((1-epsilon)/epsilon)
}
#Q4.1 my.adaboost
my.adaboost = function(x.train, y.train, x.test, T){
  require(rpart)
  len_x = nrow(x.train)
  new.train = cbind(y.train, x.train)
  w = rep(1, len_x)*1/len_x
  pred.final = rep(0, nrow(x.test))
  for(i in seq(T)){
    new.bootstrap = new.train[sample(nrow(new.train), size = len_x, replace = TRUE, prob = w), ]
    tree.boost = rpart(y.train ~., data = new.bootstrap)
    pred = sign(predict(tree.boost, x.train))
    epsilon = sum(w[pred != y.train])
    alpha = alpha_fun(epsilon)
    w = ((pred == y.train)*exp(-alpha) + (pred != y.train)*exp(alpha))*w
    w = w/sum(w)
    pred.test = sign(predict(tree.boost, x.test))
    pred.final = pred.final + pred.test*alpha
  }
  pred.final = sign(pred.final)
  return(pred.final)
}

#Q4.2 Test Data with T=15
x.train = train[, -1]
y.train = train[, 1]
y.train = (y.train == 1)*1 - (y.train == 0)*1
x.test = test[, -1]
T = 15
pred = my.adaboost(x.train, y.train, x.test, T)

y.test = test[, 1]
y.test = (y.test == 1)*1 - (y.test == 0)*1

sum(pred != y.test)/length(y.test)
#25 percent misclassified


#Q4.3 Iterations
mod.adaboost = function(x.train, y.train, x.test, y.test, T){
  require(rpart)
  len_x = nrow(x.train)
  new.train = cbind(y.train, x.train)
  error.train = c()
  error.test = c()
  w = rep(1, len_x)*1/len_x
  pred.final.train = rep(0, nrow(x.train))
  pred.final.test = rep(0, nrow(x.test))
  for(i in seq(T)){
    new.bootstrap = new.train[sample(nrow(new.train), size = len_x, replace = TRUE, prob = w), ]
    tree.boost = rpart(y.train ~., data = new.bootstrap)
    pred = sign(predict(tree.boost, x.train))
    
    epsilon = sum(w[pred != y.train])
    alpha = alpha_fun(epsilon)
    w = ((pred == y.train)*exp(-alpha) + (pred != y.train)*exp(alpha))*w
    w = w/sum(w)
    pred.final.train = pred.final.train + pred*alpha
    pred = sign(pred.final.train)
    error.train[i] = sum(pred!=y.train)
    pred.test = sign(predict(tree.boost, x.test))
    pred.final.test = pred.final.test + pred.test*alpha
    pred = sign(pred.final.test)
    error.test[i] = sum(pred!=y.test)
  }
  error = cbind(error.train,error.test)
  return(error)
}

#5 Runs

#1
error = mod.adaboost(x.train, y.train, x.test, y.test, T=50)
plot(error[,1], xlab= "T", ylab = "train error", col="red") 
plot(error[,2], xlab = "T", ylab= "test error", col = "blue")

#2
error = mod.adaboost(x.train, y.train, x.test, y.test, T=50)
plot(error[,1], xlab= "T", ylab = "train error", col="red")
plot(error[,2], xlab = "T", ylab= "test error", col = "blue")

#3
error = mod.adaboost(x.train, y.train, x.test, y.test, T=50)
plot(error[,1], xlab= "T", ylab = "train error", col="red")
plot(error[,2], xlab = "T", ylab= "test error", col = "blue")

#4
error = mod.adaboost(x.train, y.train, x.test, y.test, T=50)
plot(error[,1], xlab= "T", ylab = "train error", col="red")
plot(error[,2], xlab = "T", ylab= "test error", col = "blue")

#5
error = mod.adaboost(x.train, y.train, x.test, y.test, T=50)
plot(error[,1], xlab= "T", ylab = "train error", col="red")
plot(error[,2], xlab = "T", ylab= "test error", col = "blue")

#We get different values in each run

#Single tree classification

new.train = cbind(y.train, x.train)

#Fit tree
single.tree = rpart(y.train~., data = new.train)

#Predict on train
pred.train = predict(single.tree, x.train)
pred.train = sign(pred.train)
misclassified.train = sum(pred.train!=new.train$y.train)
print(misclassified.train) #154

#Predict on test
pred.test = predict(single.tree, x.test)
pred.test = sign(pred.test)
misclassified.test = sum(pred.test!=y.test)
print(misclassified.test) #94


#Prune Tree Classification
prune.tree = ctree(y.train~., data = new.train)

#Predict on train
prune.train = predict(prune.tree, x.train)
prune.train = sign(prune.train)
misclassified.prune.train = sum(prune.train!= new.train$y.train)
print(misclassified.prune.train) #153

#Predict on test
prune.test = predict(prune.tree, x.test)
prune.test= sign(prune.test)
misclassified.prune.test = sum(prune.test!= y.test)
print(misclassified.prune.test) #92

#The number of misclassification on train and test were less on adaboost comapared to single
#tree and prune tree classification. 


