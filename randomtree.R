library(rpart)
library(rpart.plot)

tree <- rpart(Kyphosis ~ ., method = 'class', data=kyphosis)
printcp(tree)
#plot(tree,uniform=T,main='Kyphosis Tree')
#text(tree,use.n = T,all = T)
prp(tree) #nicer visual

