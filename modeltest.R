df <- read.csv('student-mat.csv',sep=';')

# Split Data into Train and Test Set
library('caTools')
# Set A Seed
set.seed(101)
# Split up sample
sample <- sample.split(df$G3,SplitRatio = 0.7)
# 70% of data -> train 
train <- subset(df,sample == TRUE)
# 30% test data
test <- subset(df,sample == FALSE)

# Train/build model
model <- lm(G3 ~ ., data=  train)

# Predict
G3.predictions <- predict(model,test)

results <- cbind(G3.predictions,test$G3)
colnames(results) <- c('predicted','actual')
results <- as.data.frame(results)
print(head(results))

# neg values
to_zero <- function(x) {
  if (x<0) {
    return(0)
  } else {
    return(x)
  }
}

# Apply zero fn
results$predicted <- sapply(results$predicted,to_zero)

# Mean squared error
mse <- mean( (results$actual - results$predicted)^2 )
print('mse')
print(mse)

# RMSE
print("RMSE")
print(mse^0.5)

SSE <- sum( (results$predicted - results$actual)^2 )
SST <- sum( (mean(df$G3) - results$actual)^2 )

R2 <- 1 - SSE/SST
print('R2')
print(R2)
