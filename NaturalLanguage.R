############# Natural language Processing

#install.packages('tm',repos='http://cran.us.r-project.org')
#install.packages('twitteR',repos='http://cran.us.r-project.org')
#install.packages('wordcloud',repos='http://cran.us.r-project.org')
#install.packages('RColorBrewer',repos='http://cran.us.r-project.org')
#install.packages('e1017',repos='http://cran.us.r-project.org')
#install.packages('class',repos='http://cran.us.r-project.org')

library(Amelia)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(caTools)
library(rpart)
library(rpart.plot)
library(ISLR)
library(randomForest)
library(e1071)

library(twitteR)
library(tm)  ########### TEXT MANIPULATION
library(wordcloud)
library(RColorBrewer)

## connect to twitter
ckey <- 	'hQSDtxkg3tZcOYmDZaR6uzEq4'
skey <-     'IS1H9AGqQsaM3eQRCboSHmEmiEKPd5xO5TzCGAr2rm9BebxyHM'
token <-   '3195176676-VBgsgIzRG0YX9Am0Dqu5oQq84BcjHF4Id5LLxgY'
sectoken <- 'N76CNGKgPWQh5tp5QzUzlKRZIfpkxyMNx0IHfKPOSMaFf'

setup_twitter_oauth(ckey,skey,token,sectoken)
##since='2016-01-01', until='2017-06-01'

soccer.tweets <- searchTwitter('Paas',n=100,lang='en')

soccer.text <- sapply(soccer.tweets, function(x)x$getText())

######### CLEAN THE DATA ######

soccer.text <- iconv(soccer.text,'UTF-8','ASCII')
soccer.corpus <- Corpus(VectorSource(soccer.text))

##### apply term matrix. Now 
term.doc.matrix <- TermDocumentMatrix(soccer.corpus, control = list(removePunctuation = TRUE,
                                      stopwords=c('open','shift','redhat','https',stopwords('english')),removeNumbers = TRUE,tolower = TRUE ))

###### convert object into matrix
term.doc.matrix <- as.matrix(term.doc.matrix)

# get word count
word.freq <- sort(rowSums(term.doc.matrix),decreasing = TRUE)
dm <- data.frame(word=names(word.freq),freq= word.freq)

##### lets create word cloud , its data visualization 

wordcloud(dm$word,dm$freq,random.order =FALSE)
print(word.freq)



