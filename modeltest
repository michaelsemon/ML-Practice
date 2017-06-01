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

#Run Model


