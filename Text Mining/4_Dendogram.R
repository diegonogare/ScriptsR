#Ted Kwartler
#twitter: @tkwartler
#ODSC Workshop: Intro to Text Mining using R
#11-14-2015
#v4.0 Frequency Count & Dendogram

#Set the working directory
setwd('/Users/ted/Desktop/ODSC')

#libraries
library(tm)
library(ggplot2)
library(ggthemes)

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
custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'amp', 'beer')

#Dendogram coloring function
colLab <- function(n) {
  if (is.leaf(n)) {
    a <- attributes(n)
    labCol <- labelColors[clusMember[which(names(clusMember) == a$label)]]
    attr(n, "nodePar") <- c(a$nodePar, lab.col = labCol)
  }
  n
}

#bring in some text
text<-read.csv('beer.csv', header=TRUE)

#Keep the meta data, apply the functions to make a clean corpus
dd<-data.frame(id=text$id,text=text$text)
custom.reader <- readTabular(mapping=list(content="text", id="id"))
corpus <- VCorpus(DataframeSource(dd), readerControl=list(reader=custom.reader))
corpus<-clean.corpus(corpus)

#Make a Document Term Matrix or Term Document Matrix depending on analysis
tdm<-TermDocumentMatrix(corpus)
tdm.tweets.m<-as.matrix(tdm)

#Frequency Data Frame
term.freq<-rowSums(tdm.tweets.m)
freq.df<-data.frame(word=names(term.freq),frequency=term.freq)

#Save a copy
write.csv(freq.df,'term_frequency.csv', row.names=F)

#Make a barplot of the top terms
top.words<-subset(freq.df, term.freq>=90)
top.words <- top.words[order(top.words$frequency, decreasing=F),]
top.words$word<-factor(top.words$word, levels=unique(as.character(top.words$word)))
ggplot(top.words, aes(x=word, y=frequency))+geom_bar(stat="identity", fill='darkred') +coord_flip()+theme_gdocs()+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=5.0)
  
#inspect word associations
associations<-findAssocs(tdm, 'zombie', 0.30)[,1]
terms<-row.names(findAssocs(tdm,'zombie',0.30))
a.df<-data.frame(associations,terms)
a.df$terms<-factor(a.df$terms, levels=a.df$terms)
ggplot(a.df, aes(y=terms)) + geom_point(aes(x=associations), data=a.df)+
  theme_gdocs()+geom_text(aes(x=associations,label=associations), colour="red",hjust=-.25)

#Hierarchical Clustering
tdm2 <- removeSparseTerms(tdm, sparse=0.95) #shoot for ~40 terms
tdm2.df<-as.data.frame(inspect(tdm2))
hc <- hclust(dist(tdm2.df))
hcd <- as.dendrogram(hc)
clusMember <- cutree(hc, 4)
labelColors <- c("#CDB380", "#036564", "#EB6841", "#EDC951")
clusDendro <- dendrapply(hcd, colLab)
plot(hc,yaxt='n')
plot(clusDendro, main = "Hierarchical Dendrogram", type = "triangle",yaxt='n')

#End