library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)
data <- Boston

#normalize data
maxs <- apply(data,2,max) # margin = 2 for columns-- returning maxes
mins <- apply(data,2,min)
scaled.data <- scale(data,center=mins,scale=maxs-mins)
scaled <- as.data.frame(scaled.data)

# train/test sets
split <- sample.split(scaled$medv, SplitRatio = 0.7)
train <- subset(scaled, split==T)
test <- subset(scaled,split==F)

# neural net 
n <- names(train)
f <- as.formula(paste("medv~",paste(n[!n %in% "medv"],collapse = " + "))) # cheat to get the features

nn <- neuralnet(f,data=train,hidden = c(5,3),linear.output = TRUE) # layers of 5 neurons and 3 neurons
#nn <- neuralnet(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat,data=train...
  print(plot(nn)) # plot neural network

# predictions
predicted.nn.values <- compute(nn,test[1:13])
#str(predicted.nn.values)
true.predictions <- predicted.nn.values$net.result * (max(data$medv)-min(data$medv))+min(data$medv)
test.r <- (test$medv)*(max(data$medv)-min(data$medv))+min(data$medv) # convert the test data
MSE.nn <- sum((test.r - true.predictions)^2)/nrow(test)
error.df <- data.frame(test.r,true.predictions)
nn.error.pl <- ggplot(error.df,aes(x=test.r,y=true.predictions)) + geom_point() + stat_smooth()





