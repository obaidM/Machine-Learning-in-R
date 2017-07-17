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

obaid.tweets <- searchTwitter('lebron',lang='en', since='2016-01-01', until='2017-06-01',n=10000)

obaid.text <- sapply(soccer.tweets, function(x)x$getCount())