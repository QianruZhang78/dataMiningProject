
require(caTools)
library(randomForest)

x = read.csv('/Users/zz/Desktop/corona_tested_individuals_ver_0083.english.csv',nrows = 5000)
x <- x[2:7]  #drop dates and columns that will not be used
x
colSums(is.na(x)) #checking for missing values
sample = sample.split(data$corona_result, SplitRatio = .75) #split training and testing set
train = subset(x, sample == TRUE)
test  = subset(x, sample == FALSE)
summary(test)  
str(train)     #2500 objects
str(test)      #7500 objects
rf <- randomForest(                   #build and train the model with traning set 
  as.factor(corona_result) ~ .,
  data = train,
  na.action = na.exclude
)
print(rf)  #result for training set
pred = predict(rf, newdata=test[-6])  #prediction on testing set
cm = table(test[,6],pred)  #confusion matrix on testing set
cm
importance(rf)   #importance of features
varImpPlot(rf)   #plot of imprtance of features

