# LOAD TWITTER PACKAGE
library(tm)
require(twitteR)
require(ROAuth)
library(wordcloud)

# LOAD TWITTER CREDENTIALS
api.key = "Jgw0oLUmjFenqkmxPIvvGW4Px"
api.secret = "4l1YKTabgg6vZp0g8mjAccXb1LpTRxMze3VxbETWbgwCj4poB4"

setup_twitter_oauth(api.key, api.secret, 
                    access_token = NULL, access_secret = NULL)

# A. Get Tweets for Gainers
t.CAPN <- searchTwitter('$CAPN', n = 100)
t.XGTI <- searchTwitter('$XGTI', n = 100)
t.EBIO <- searchTwitter('$EBIO', n = 100)
save(t.CAPN, file="t.CAPN")
save(t.XGTI, file="t.XGTI")
save(t.EBIO, file="t.EBIO")
gainers <- c(t.CAPN,t.XGTI,t.EBIO)

#Get Tweets for Losers
t.BGMD <- searchTwitter('$BGMD', n = 100)
t.ENZN <- searchTwitter('$ENZN', n = 100)
t.IMDZ <- searchTwitter('$IMDZ', n = 100)
save(t.BGMD, file="t.BGMD")
save(t.ENZN, file="t.ENZN")
save(t.IMDZ, file="t.IMDZ")
losers <- c(t.BGMD,t.ENZN,t.IMDZ)

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
 
getSymbols("CAPN")  
getSymbols("EBIO")
 
CAPN.dframe = data.frame(CAPN['2016-04-20'])
EBIO.dframe = data.frame(EBIO['2016-04-20'])
 
Open = c(as.numeric(CAPN.dframe$CAPN.Open),as.numeric(EBIO.dframe$EBIO.Open))
High = c(as.numeric(CAPN.dframe$CAPN.High),as.numeric(EBIO.dframe$EBIO.High))
Low = c(as.numeric(CAPN.dframe$CAPN.Low),as.numeric(EBIO.dframe$EBIO.Low))
Close = c(as.numeric(CAPN.dframe$CAPN.Close),as.numeric(EBIO.dframe$EBIO.Close))
Volume = c(as.numeric(CAPN.dframe$CAPN.Volume),as.numeric(EBIO.dframe$EBIO.Volume))
Adjusted = c(as.numeric(CAPN.dframe$CAPN.Adjusted),as.numeric(EBIO.dframe$EBIO.Adjusted))

stocks.data = data.frame(Stocks=c("CAPN","EBIO"),Open,High,Low,Close)


chart1 <- gvisBarChart(stocks.data)
plot(chart1)
print(chart1, tag="chart", file="chart1.html")
chart2 <- gvisColumnChart(stocks.data)
plot(chart2)
print(chart2,tag = "chart",file="chart2.html")
 
 