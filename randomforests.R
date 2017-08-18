library(randomForest)

rf.model <- randomForest(Kyphosis ~ ., data=kyphosis)
print(rf.model)