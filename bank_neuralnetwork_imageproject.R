library(MASS)
library(caTools)
library(neuralnet)
library(ggplot2)

bank <- read.csv('bank_note_data.csv')

##
# explore data
##  

# train test split
set.seed(101)
split <- sample.split(bank,0.7)
train <- subset(bank,split==T)
test <- subset(bank,split==F)

# build neural net
nn <- neuralnet(Class ~ Image.Var+Image.Skew+Image.Curt+Entropy,data=train,linear.output = FALSE, hidden=10)

# predictions - nn
predicted.bank <- compute(nn,test[,1:4])
head(predicted.bank$net.result)
predictions <- sapply(predicted.bank$net.result,round)
head(predictions)
pred.table <- table(predictions,test$Class)
print(pred.table)

# compare models- random forest
library(randomForest)

bank.factored <- bank
bank.factored$Class <- factor(bank.factored$Class)
set.seed(101)
split2 <- sample.split(bank.factored$Class,SplitRatio = 0.7)
train <- subset(bank.factored,split2==T)
test <- subset(bank.factored,split2==F)

rf.model <- randomForest(Class ~ Image.Var + Image.Skew + Image.Curt + Entropy,data=train)
rf.predict <- predict(rf.model,test)
rf.prediction.table <- table(rf.predict,test$Class)
print(rf.prediction.table)









