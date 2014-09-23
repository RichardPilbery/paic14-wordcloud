# Analyse Tweets with hastag PAIC2014
library(twitteR)
library(ROAuth)
library(tm)
library(ggplot2)
library(stringr)
library(SnowballC)
library(wordcloud)

# Instead of having to register with Twitter to obtain access to the developer API, you can simply load in PAIC2014-tweets.csv into a data frame called: tweets.df

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "http://api.twitter.com/oauth/authorize"
consumerKey <- "YOUR_API_KEY"
consumerSecret <- "YOUR_API_SECRET"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret, requestURL=reqURL, accessURL=accessURL, authURL=authURL)
twitCred$handshake()
registerTwitterOAuth(twitCred)

tweets <- searchTwitter("#PAIC2014",n=10000,since='2014-09-19',until='2014-09-21')
numTweets <- length(tweets)

#Convert to data frame
tweets.df <- twListToDF(tweets)

#Have now saved this to CSV file so could just load it in (or use RStudio convenience functions)

usableText <- str_replace_all(tweets.df$text,"[^[:graph:]]", " ") 

myCorpus <- Corpus(VectorSource(usableText))

# lower case 
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
# remove numbers
#myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*","",x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# Add stop words
myStopwords <- c(stopwords("en"),"available","via","amp","rt","fwd","will","looking")
# remove stopwords
myCorpus <- tm_map(myCorpus, content_transformer(removeWords), myStopwords)

myCorpus <- tm_map(myCorpus, content_transformer(stemDocument))

# Get frequency of words in corpus.  Convert into data.frame and then subset to only include words with minimum frequency
tdm <- TermDocumentMatrix(myCorpus)
m1 <- as.matrix(tdm)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word=names(v1),freq=v1)
over20d1 <- subset(d1,freq>25)

# Create PDF wordcloud
pdf("wordcloud.pdf", w=7, h=7)
wordcloud(over20d1$word, over20d1$freq, scale = c(8,0.6), random.order = FALSE, rot.per = .1, colors = brewer.pal(8, "Dark2"),use.r.layout=TRUE,random.color=T)
dev.off()

# Find out who tweets the most
# Source: http://gettinggeneticsdone.blogspot.com.au/2013/05/automated-analysis-tweets-bioinformatics-twitterchive.html

library('plyr')
numTweets <- count(tweets.df,c("screenName"))
users <- numTweets[order(numTweets$freq, decreasing=T), ]
users <- head(users, 40)
head(users)
pdf("barplot-top-users.pdf", w=8, h=10)
par(mar=c(5,10,4,2))
with(users[order(users$freq), ], barplot(freq, names=screenName, horiz=T, col=brewer.pal(9,'Spectral'), las=1, cex.names=0.8, cex.axis=1.0, main="Top 40 Twitterers of PAIC 2014"))
dev.off()
  