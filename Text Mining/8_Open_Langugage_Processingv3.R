#Ted Kwartler
#twitter: @tkwartler
#ODSC Workshop: Intro to Text Mining using R
#11-14-2015
#v8.2 Named Entity Recognition

#Set the working directory
setwd('/Users/ted/Desktop/ODSC')

#options, functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

#libraries
library(qdap)
library(openNLP)
library(tm)
library(GuardianR)

#Get Text
key<-'baus649ecqq8kcjhzmjrvnt8'
text <- get_guardian("Pakistan", from.date="2015-11-01",to.date="2015-11-08",api.key=key)
text.s<-as.String(text$body)

#Get Text
text <- read.csv('chardonnay.csv')
text.s<-as.String(text$text)

#Preprocessing/Encoding 
text.s<-iconv(text.s, "latin1", "ASCII", sub="")
text.s<-gsub('http\\S+\\s*', '', text.s) #remove URLs
text.s<-bracketX(text.s, bracket="all") #remove html and phrases in between brackets
text.s<-replace_abbreviation(text.s) # replaces a.m. to AM etc

#OpenNLP Annotators
persons <- Maxent_Entity_Annotator(kind = 'person')
locations <- Maxent_Entity_Annotator(kind = 'location')
organizations <- Maxent_Entity_Annotator(kind = 'organization')
sent.token.annotator <- Maxent_Sent_Token_Annotator(language = "en")
word.token.annotator <- Maxent_Word_Token_Annotator(language = "en")
pos.tag.annotator <- Maxent_POS_Tag_Annotator(language = "en")

#annotate text
annotations <- annotate(text.s,
                        list(sent.token.annotator,word.token.annotator,pos.tag.annotator,
                             persons,locations,organizations))

text.annotations<-AnnotatedPlainTextDocument(text.s,annotations)

#Extract Entities
entities <- function(doc, kind) {
  s <- doc$content
  a <- annotations(doc)[[1]]
  if(hasArg(kind)) {
    k <- sapply(a$features, `[[`, "kind")
    s[a[k == kind]]
  } else {
    s[a[a$type == "entity"]]
  }
}

people<-entities(text.annotations, kind = "person")
locations<-entities(text.annotations, kind = "location")
organization<-entities(text.annotations, kind = "organization")

#End