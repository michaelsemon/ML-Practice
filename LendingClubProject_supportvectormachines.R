library(ggplot2)
library(ggthemes)
library(e1071)
library(caTools)

loans <- read.csv('loan_data.csv')
loans$credit.policy <- factor(loans$credit.policy)
loans$inq.last.6mths <- factor(loans$inq.last.6mths)
loans$delinq.2yrs <- factor(loans$delinq.2yrs)
loans$pub.rec <- factor(loans$pub.rec)
loans$not.fully.paid <- factor(loans$not.fully.paid)

#Data Vis
fico <- ggplot(loans,aes(x=loans$fico)) + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha=0.5)
fico <- fico + scale_fill_manual(values = c('green','red')) + theme_bw()
  #print(fico)
purpose.pl <- ggplot(loans,aes(x=factor(purpose))) + geom_bar(aes(fill=not.fully.paid),position = "dodge")
purpose.pl <- purpose.pl + theme_bw()
  #print(purpose.pl)
fico.intrate <- ggplot(loans,aes(x=int.rate,y=fico)) + geom_point(aes(color=not.fully.paid),alpha=0.3) + theme_bw()
  print(fico.intrate)

# Build model
set.seed(101)
sample <- sample.split(loans$not.fully.paid,SplitRatio = 0.7)
train <- subset(loans,sample==T)
test<- subset(loans,sample==F)

svm.model <- svm(not.fully.paid ~ ., data=train)
predicted.paid <- predict(svm.model,test[1:13])
paid.matrix <- table(predicted.paid,test$not.fully.paid)

# Tune the model
#tune.results.paid <- tune(svm,train.x=not.fully.paid~., data=train,kernel='radial',ranges=list(cost=c(1,10),gamma=c(0.1)))
model<- svm(not.fully.paid ~., data=train,cost=10,gamma=0.1)
predicted.final <- predict(model,test[1:13])
final.table <- table(predicted.final,test$not.fully.paid)



