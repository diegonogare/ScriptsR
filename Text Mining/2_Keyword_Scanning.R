#Ted Kwartler
#twitter: @tkwartler
#ODSC Workshop: Intro to Text Mining using R
#11-14-2015
#v2.0 Basics: Keyword Scanning

#Set the working directory
setwd('/Users/ted/Desktop/ODSC')

#libraries
library(stringi)

#options, functions
options(stringsAsFactors = FALSE) #text strings will not be factors of categories
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

#bring in some text
text<-read.csv('coffee.csv', header=TRUE)

#logical T/F vector that a string appears at least ONCE
coffee<-grepl("coffee", text$text, ignore.case=TRUE)
starbucks<-grepl("starbucks", text$text, ignore.case=TRUE)

#find the row positions of a specific word appearing at least ONCE
#this shows the difference between grep and grepl
grep("mug", text$text, ignore.case=TRUE)

#logical T/F for one word OR another appears at least ONCE
keywords<-"mug|glass|cup"
mug.glass.cup<-grepl(keywords, text$text,ignore.case=TRUE)

#calculate the % of times among all tweets
sum(coffee)/nrow(text)
sum(starbucks)/nrow(text)
sum(mug.glass.cup)/nrow(text)

#count occurences of words per tweet
hot.coffee<-stri_count(text$text, fixed="hot")
sum(hot.coffee)/nrow(text)

#example data export
keyword.scans<-data.frame(text,coffee,starbucks,mug.glass.cup)
write.csv(keyword.scans,'grepl_example.csv', row.names=FALSE)

#End


