library(rpart)
library(rpart.plot)
rm(list=ls())
data = read.csv("corona_tested_individuals_ver_0083.english (1).csv",stringsAsFactors=FALSE, sep=",", nrows= 10000)[-c(2)]
#Input the first 10,0000 elements from csv for the sake of brevity
set.seed(1234)
ins <- sample(2, nrow(data), replace = T, prob = c(0.75, 0.25))
#sample and split the data into 2 bins(test, train)
train <- data[ins == 1,]
test <- data[ins == 2,]
#generate tree with corona result as the target attribute
tree <- rpart(corona_result ~., data = train, method="class", control=rpart.control(minsplit=2, minbucket=1, cp=0.001), parms=list(split='information'))
#split in accordance with information gain
printcp(tree)

fit <- predict(tree, test, type="class")
mat <- table(test$corona_result, fit)
accuracy_Test <- sum(diag(mat)) / sum(mat)
#generate confusion matrix
print(paste('Accuracy for test', accuracy_Test))
#return accuracy
rpart.plot(tree)

#prune 
tree1 <- prune(tree, cp = .03)
out <- predict(tree1, test, type="class")
mat1 <- table(test$corona_result, out)
#retest accuracy
accuracy_Test1 <- sum(diag(mat1)) / sum(mat1)
print(paste('Accuracy for test', accuracy_Test1))



