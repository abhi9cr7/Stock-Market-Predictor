# LOAD TWITTER PACKAGE
library(tm)
require(twitteR)
require(ROAuth)
library(wordcloud)
library(rtweet)

# LOAD TWITTER CREDENTIALS
appname='CS688'
key = "Jgw0oLUmjFenqkmxPIvvGW4Px"
secret = "4l1YKTabgg6vZp0g8mjAccXb1LpTRxMze3VxbETWbgwCj4poB4"
access_token='3039950507-cCzRqhwYTsfC2Sxvq6CsY9yKFblRs0PyZYVCUQv'
access_secret='D1v68mQBMWUhayowXEPnrWicQqj8ioIzfQy88UakplIBU'

create_token(
   app = appname,
   consumer_key = key,
   consumer_secret = secret,
   access_token = access_token,
   access_secret = access_secret
)



setup_twitter_oauth(api.key, api.secret, 
                    access_token = NULL, access_secret = NULL)

# A. Get Tweets for Gainers
t.GOOG <- search_tweets('$GOOG', n = 100,include_rts = FALSE)
t.AAPL <- search_tweets('$AAPL', n = 100,include_rts=FALSE)
t.FB <- search_tweets('$FB', n = 100,include_rts=FALSE)
save(t.GOOG, file="t.GOOG")
save(t.AAPL, file="t.AAPL")
save(t.FB, file="t.FB")
gainers <- c(t.GOOG,t.AAPL,t.FB)

#Get Tweets for Losers
t.VRTX <- search_tweets('$VRTX', n = 100)
t.MSFT <- search_tweets('$MSFT', n = 100)
t.NVDA <- search_tweets('$NVDA', n = 100)
save(t.VRTX, file="t.VRTX")
save(t.MSFT, file="t.MSFT")
save(t.NVDA, file="t.NVDA")
losers <- c(t.VRTX,t.MSFT,t.NVDA)

#B) Create corpora for tweets
get.corpus <- function (tweets) 
{
  tweets.text <- lapply(tweets, function(t) {t$getText()})
  data.source <- VectorSource(tweets.text)
  data.corpus <- Corpus(data.source)
}

data.corpus.gainers <- get.corpus(gainers)
writeCorpus(data.corpus.gainers,path = "Corpus.Gainers")

data.corpus.losers <- get.corpus(losers)
writeCorpus(data.corpus.losers,path= "Corpus.Losers")

#C Pre-Processing
trans.corpus <- function (data.corpus) {
  data.corpus <- tm_map(data.corpus, content_transformer(removePunctuation))
  data.corpus <- tm_map(data.corpus, content_transformer(tolower))
  data.corpus <- tm_map(data.corpus, removeNumbers)
  english.stopwords <- stopwords("en")
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  data.corpus <- tm_map(data.corpus, content_transformer(removeURL))
  data.corpus <- tm_map(data.corpus,content_transformer(removeWords),english.stopwords)
  data.corpus <- tm_map(data.corpus,content_transformer(stemDocument))
  data.corpus <- tm_map(data.corpus,content_transformer(stripWhitespace))
  
}

data.corpus.gainers <- trans.corpus(data.corpus.gainers)
data.corpus.losers <- trans.corpus(data.corpus.losers)

#D TDM for each corpus
tdm.Gainers <- TermDocumentMatrix(data.corpus.gainers)
tdm.Losers <- TermDocumentMatrix(data.corpus.losers)
 
save(tdm.Gainers, file = "tdm1")
save(tdm.Losers, file = "tdm2")

#E Frequent terms
findFreqTerms(tdm.Gainers, lowfreq=15)
findFreqTerms(tdm.Losers, lowfreq=15)
m.Gainers <-as.matrix(tdm.Gainers)
m.Losers <- as.matrix(tdm.Losers)
frq.Gainers <- rowSums(m.Gainers)
frq.Gainers <- sort(frq.Gainers,decreasing = T)
frq.Losers <- rowSums(m.Losers)
frq.Losers <- sort(frq.Losers,decreasing = T)
cbind(frq.Gainers[1:10])
cbind(frq.Losers[1:10])

palette <- brewer.pal(8,"Dark2")
set.seed(123)
wordcloud(words=names(frq.Gainers),
           scale = c(3, 0.4),
           freq = frq.Gainers,
           min.freq = 10,
           random.order = F,
           colors = palette)
wordcloud(words=names(frq.Losers),
           scale = c(4, 0.4),
           freq = frq.Losers,
           min.freq = 10,
           random.order = F,
           colors = palette)


#F Sentiment Analysis
sentiment <- function(text, pos.words, neg.words) {
   text <- gsub('[[:punct:]]', '', text)
   text <- gsub('[[:cntrl:]]', '', text)
   text <- gsub('\\d+', '', text)
   text <- tolower(text)
   # split the text into a vector of words
   words <- strsplit(text, '\\s+')
   words <- unlist(words)
   # find which words are positive
   pos.matches <- match(words, pos.words)
   pos.matches <- !is.na(pos.matches)
   # find which words are negative
   neg.matches <- match(words, neg.words)
   neg.matches <- !is.na(neg.matches)
   # calculate the sentiment score
   p <- sum(pos.matches)
   n <- sum(neg.matches)
   if (p == 0 & n == 0)
     return (NA)
   else
     return (p - n)
 }
pos.words <- scan('positive-words.txt',
                   what='character',
                   comment.char = ';')
neg.words <- scan('negative-words.txt',
                   what='character',
                   comment.char = ';')
# Fetch texts
Gainers.texts <- 
   lapply(gainers, 
          function(t) {
            iconv(t$getText(), 
                  "latin1", "ASCII", sub="")
          })
 
Losers.texts <- 
   lapply(losers, 
          function(t) {
            iconv(t$getText(), 
                  "latin1", "ASCII", sub="")
          })
 
 
# Scores
Gainers.scores <- sapply(Gainers.texts, 
                          sentiment, 
                          pos.words, neg.words)
 
Losers.scores <- sapply(Losers.texts, 
                         sentiment, 
                         pos.words, neg.words)
 
 
table(Gainers.scores)
 
table(Losers.scores)
 
barplot(table(Gainers.scores), main="Sentiment Analysis for GAINERS",
         xlab="Score", ylab="Count", ylim=c(0,54), col="green")
barplot(table(Losers.scores), main="Sentiment Analysis for Losers",
         xlab="Score", ylab="Count", ylim=c(0,20), col="green")
 
# G GogleVis
library(quantmod)
library(googleVis)
 
getSymbols("GOOG")  
getSymbols("FB")
 
GOOG.dframe = data.frame(GOOG['2016-04-20'])
FB.dframe = data.frame(FB['2016-04-20'])
 
Open = c(as.numeric(GOOG.dframe$GOOG.Open),as.numeric(FB.dframe$FB.Open))
High = c(as.numeric(GOOG.dframe$GOOG.High),as.numeric(FB.dframe$FB.High))
Low = c(as.numeric(GOOG.dframe$GOOG.Low),as.numeric(FB.dframe$FB.Low))
Close = c(as.numeric(GOOG.dframe$GOOG.Close),as.numeric(FB.dframe$FB.Close))
Volume = c(as.numeric(GOOG.dframe$GOOG.Volume),as.numeric(FB.dframe$FB.Volume))
Adjusted = c(as.numeric(GOOG.dframe$GOOG.Adjusted),as.numeric(FB.dframe$FB.Adjusted))

stocks.data = data.frame(Stocks=c("GOOG","FB"),Open,High,Low,Close)


chart1 <- gvisBarChart(stocks.data)
plot(chart1)
print(chart1, tag="chart", file="chart1.html")
chart2 <- gvisColumnChart(stocks.data)
plot(chart2)
print(chart2,tag = "chart",file="chart2.html")
 
 