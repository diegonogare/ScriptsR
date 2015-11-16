#Ted Kwartler
#twitter: @tkwartler
#ODSC Workshop: Intro to Text Mining using R
#11-14-2015
#1.0 Install Packages

install.packages(c('tm', 'stringi','ggplot2','ggthemes','wordcloud','RColorBrewer',
                   'plyr','stringr','topicmodels','portfolio','openNLP', 'qdap','SnowballC'), 
                 repos = "http://cran.r-project.org", 
                 dependencies = c("Depends", "Imports", "Suggests"))

install.packages('openNLPmodels.en', repos = "http://datacube.wu.ac.at/", type = "source")

#Depending on your machine, you may be able to use parallel processing. 
#For windows users, add the package "doParallel" to the list above. 
#Mac and Linux uses can add "doMC" instead.

#End