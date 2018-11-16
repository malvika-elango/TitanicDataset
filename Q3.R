library("rpart")
library("rpart.plot")


setwd("C:/Users/Malvika/Documents/Machine Learning/HW5")
titanic.train = read.csv("titanic_train.csv")
titanic.test = read.csv("titanic_test_labeled.csv")

#Q3.1 Variable Selection

#Variables to exclude: Passenger ID, Name, Ticket, Cabin
#Since the above variables are not useful in predicting the final outcomes (survived or not),
#they can be excluded when fitting the tree

#Q3.2 Missing Values 

#Examine missing values (Age)

age.missing = which(is.na(titanic.train$Age))
mean(titanic.train$Survived[age.missing]) #29% of people with missing age survived
mean(titanic.train$Survived[-age.missing])#40% of people with age survived

#Decision tree with missing values of Age
tree = rpart(Survived ~ Pclass+Sex+ Age+ SibSp+Parch+Fare+ Embarked, data=titanic.train)
rpart.plot(tree)
predict.train = predict(tree, titanic.train)
rmse.train= sqrt(mean((titanic.train$Survived - predict.train)^2))
print(rmse.train) #35%


#Decision tree after replacing Age with mean values
titanic.train$Age[titanic.train$Survived == 1 & is.na(titanic.train$Age)] = mean(titanic.train$Age[titanic.train$Survived == 1 & !is.na(titanic.train$Age)])
titanic.train$Age[titanic.train$Survived == 0 & is.na(titanic.train$Age)] = mean(titanic.train$Age[titanic.train$Survived == 0 & !is.na(titanic.train$Age)])
tree.new = rpart(Survived ~ Pclass+Sex+ Age+ SibSp+Parch+Fare+ Embarked, data=titanic.train)
rpart.plot(tree.new)
predict.train = predict(tree.new, titanic.train)
rmse.train= sqrt(mean((titanic.train$Survived - predict.train)^2))
print(rmse.train) #32%

#Since RMSE decreases after replacing the missing values of Age with respective means (survived or not)
#this is a good technique to use. 

#Q3.3 Although, we were able to fit the decision tree with missing values, this will not be
#the case for linear models. Therefore we have to replace the missing values and the same
#method as above can be used. 
