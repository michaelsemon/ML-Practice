library(ggplot2)
library(ggthemes)

df1 <- read.csv('winequality-red.csv',sep=';')
df2 <- read.csv('winequality-white.csv',sep=';')
df1$label <- sapply(df1$pH, function(x){'red'})
df2$label <- sapply(df2$pH, function(x){'white'})
wine <- rbind(df1,df2)

### Data Vis ###
# Histogram of residual sugar by wine color
sugar.pl <- ggplot(wine,aes(residual.sugar)) + geom_histogram(aes(fill=label),color='black',bins=50)
sugar.pl <- sugar.pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()
    #print(sugar.pl)
#Histogram of citric.acid by wine color
citric.acid.pl <- ggplot(wine,aes(citric.acid)) + geom_histogram(aes(fill=label),color='black',bins=50)
citric.acid.pl <- citric.acid.pl + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()
    #print(citric.acid.pl)
#Histrogram of alcohol by wine color
alcohol <- ggplot(wine,aes(alcohol)) + geom_histogram(aes(fill=label),color='black',bins=50)
alcohol <- alcohol + scale_fill_manual(values = c('#ae4554','#faf7ea')) + theme_bw()
    #print(alcohol)
#Scatterplot of residual.sugar vs citric.acid by wine color
acid.sugar <- ggplot(wine,aes(citric.acid,residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
acid.sugar <- acid.sugar + theme_dark() 
  #print(acid.sugar)
#Scatterplot of volatile.acidity versus residual.sugar by wine color
acidity.sugar <- ggplot(wine,aes(volatile.acidity,residual.sugar)) + geom_point(aes(color=label),alpha=0.2)
acidity.sugar <- acidity.sugar + theme_dark() 
    #print(acidity.sugar)
  

### Building the Clusters ###
clus.data <- wine[,1:12]

wine.cluster <- kmeans(wine[1:12],2)
print(wine.cluster$centers)

### Evaluating the Clusters ###
wine.results <- table(wine$label,wine.cluster$cluster)
print(wine.results)



