library(twitteR)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Connect to twitter
ckey <- 'oBUUMAkS2S8TkLNnSgh0u7v0M'
skey <- '8UKiKxMlProGgYs9PRZfxlNThd7bIvkz2KSAizyNVTccCJbSq1'
atoken <- '58075264-mz3776KVxyYxe8Lb97K6LYj0XnXyI2O276peikcV8'
atokensec <- '7zmxDt3QxVMfJ5JBxSIMPtxXm0phonAfaVGUf12JKnoXg'
setup_twitter_oauth(ckey,skey,atoken,atokensec)

# return tweets
soccer.tweets <- searchTwitter('python',n=1000,lang  = 'en')
soccer.text <- sapply(soccer.tweets,function(x) x$getText()) # get text from tweets
# clean text data
soccer.text <- iconv(soccer.text,'UTF-8','ASCII')
soccer.corpus <- Corpus(VectorSource(soccer.text))

# Document Term Matrix
term.doc.matrix <- TermDocumentMatrix(soccer.corpus,
                                      control=list(removePunctuation=TRUE,
                                                   stopwords=c('python','https',stopwords('english')),removeNumbers=TRUE,
                                                   tolower=TRUE))

# Convert object to a matix
term.doc.matrix <- as.matrix(term.doc.matrix)

# Get word counts
word.freq <- sort(rowSums(term.doc.matrix),decreasing=T)
df <- data.frame(word=names(word.freq),freq=word.freq)

# Create the wordcloud
wordcloud(df$word,df$freq,random.order=FALSE,colors=brewer.pal(8,'Dark2'))



