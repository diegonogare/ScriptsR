#Ted Kwartler
#twitter: @tkwartler
#ODSC  Workshop: Intro to Text Mining using R
#11-14-2015
#v6.0 Other Wordclouds

#Set the working directory
setwd('/Users/ted/Desktop/ODSC')

#libraries
library(tm)
library(wordcloud)
library(RColorBrewer)

#options, functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

#try to lower function
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

#read CSV
data1 <- read.csv(file="chardonnay.csv", head=TRUE, sep=",")
data2 <- read.csv(file="coffee.csv", head=TRUE, sep=",")
data3 <- read.csv(file="beer.csv", head=TRUE, sep=",")

#encoding fix
tweets1<-iconv(data1$text, "latin1","ASCII",sub='')
tweets2<-iconv(data2$text, "latin1","ASCII",sub='')
tweets3<-iconv(data3$text, "latin1","ASCII",sub='')

#create a corpus for each group refencing the columns
item1 <- VCorpus(VectorSource(tweets1))
item2 <- VCorpus(VectorSource(tweets2))
item3 <- VCorpus(VectorSource(tweets3))

#remove punctuation
item1 <- tm_map(item1, removePunctuation)
item2 <- tm_map(item2, removePunctuation)
item3 <- tm_map(item3, removePunctuation)

#remove numbers
item1 <- tm_map(item1, removeNumbers)
item2 <- tm_map(item2, removeNumbers)
item3 <- tm_map(item3, removeNumbers)

#make lowercase
item1 <- tm_map(item1, tryTolower)
item2 <- tm_map(item2, tryTolower)
item3 <- tm_map(item3, tryTolower)

#custom stopwords and remove them
my_stopwords <- c(stopwords('english'), 'chardonnay', 'beer', 'coffee', 'amp','rt')
item1 <- tm_map(item1, removeWords, my_stopwords)
item2 <- tm_map(item2, removeWords, my_stopwords)
item3 <- tm_map(item3, removeWords, my_stopwords)

#combine the columns into a single vector and make into a single corpus
group1 <- paste(item1, collapse=" ")
group2 <- paste(item2, collapse=" ")
group3 <- paste(item3, collapse=" ")
all <- c(group1, group2, group3)
corpus <- VCorpus(VectorSource(all))

#build the combined TDM
tdm <- TermDocumentMatrix(corpus)

#convert the vector to a matrix
tdm <- as.matrix(tdm)

#label the matrix columns in the same order you brought them in
colnames(tdm) = c("Chardonnay", "Coffee", "Beer")
tdm[50:55,1:3]

#look at all available color pallettes
display.brewer.all()

#set the pallette you want by putting in between quotes
pal <- brewer.pal(8, "Purples")
pal <- pal[-(1:2)]

#create the word in common word cloud
commonality.cloud(tdm, max.words=300, random.order=FALSE,colors=pal)

#create the comparison word cloud
set.seed(1237)
comparison.cloud(tdm, max.words=200, random.order=FALSE,title.size=1.0,colors=brewer.pal(ncol(tdm),"Dark2"))

#End