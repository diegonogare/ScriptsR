#Ted Kwartler
#twitter: @tkwartler
#ODSC Workshop: Intro to Text Mining using R
#11-14-2015
#v7.3 Topic Modeling, Sentiment and Length Treemap

#Set the working directory
setwd('/Users/ted/Desktop/ODSC')

#libraries
library(treemap)
library(portfolio)
library(qdap)
library(GuardianR)
library(topicmodels)
library(tm)
library(SnowballC)

#options, functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

tryTolower <- function(x){
  # return NA when there is an error
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error = function(e) e)
  # if not an error
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

clean.corpus<-function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, custom.stopwords)
  return(corpus)
}

#Create custom stop words
custom.stopwords <- c(stopwords('english'), 'pakistan', 'gmt','pm')

#Get Text
key<-'baus649ecqq8kcjhzmjrvnt8'
text <- get_guardian("Pakistan", from.date="2015-11-01",to.date="2015-11-08",api.key=key)

#Preprocessing/Encoding 
body<-iconv(text$body, "latin1", "ASCII", sub="")
body<-gsub('http\\S+\\s*', '', body) #remove URLs
body<-bracketX(body, bracket="all") #remove html and phrases in between parenteses
body<-replace_abbreviation(body) # replaces a.m. to AM etc

#Preprocessed df
text.body<-data.frame(id=text$id,text=body)

##Keep the meta data, apply the functions to make a clean corpus
custom.reader <- readTabular(mapping=list(content="text", id="id"))
corpus <- VCorpus(DataframeSource(text.body), readerControl=list(reader=custom.reader))
corpus<-clean.corpus(corpus)

#Make a DTM
dtm<-DocumentTermMatrix(corpus)

#Remove any docs with all zeros after removing stopwords, sometimes happens with tweets
row.totals <- apply(dtm , 1, sum) 
dtm  <- dtm[row.totals> 0, ]
text.body$row.totals <-row.totals
text.body <- text[row.totals> 0, ]

#Correlated Topic Model
topic.model <- CTM(dtm, k = 5) 

#Topic Extraction
topics<-get_terms(topic.model, 12)
colnames(topics)<-c("topic1","topic2","topic3","topic4","topic5")
topics<-as.data.frame(topics)
t1<-paste(topics$topic1,collapse=' ') 
t2<-paste(topics$topic2,collapse=' ') 
t3<-paste(topics$topic3,collapse=' ') 
t4<-paste(topics$topic4,collapse=' ') 
t5<-paste(topics$topic5,collapse=' ') 

#Score each article's probability for the topic models
scoring<-posterior(topic.model)
scores<-scoring$topics
scores<-as.data.frame(scores)
colnames(scores)<-c(t1,t2,t3,t4,t5)

#Find the max prob and return the class
topics.text<-as.data.frame(cbind(row.names(scores),apply(scores,1,function(x) names(scores)[which(x==max(x))]))) 

#Add polarity sentiment
sentiment.score<-polarity(text$body)

#Create a unified data frame
all<-cbind(topics.text,scores,sentiment.score[[1]][3], row.totals)
names(all)[2]<-paste("topic")
names(all)[8]<-paste("sentiment")
names(all)[9]<-paste("length")
all[all == ""] <- NA
colnames(all)[1] <- "id"

set.seed(1237)
treemap(all,index=c("topic","length"),vSize="length",vColor="sentiment", type="value", title="Guardan Articles mentioning Pakistan",
        palette=c("red","white","green"))
map.market(id=all$id, area=all$length, group=all$topic, color=all$sentiment, main="Sentiment/Color, Length/Area, Group/Topic")
