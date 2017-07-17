df <- read.csv('bikeshare.csv')
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
# check for any null values
null.ans <- anyNA(df)
# print(null.ans)
#EXPLORTARY  DATA ANALYSIS -- LETS PLOT SOME AND understand soem data 

pl1 <- ggplot(df,aes(x=temp,y=count)) + geom_point(alpha = 0.4,aes(color=temp))+ theme_bw()
pl1 <- pl1 + scale_color_gradient(low='green',high ='red')
#print(pl)

## convert to time stamp format

df$datetime <- as.POSIXct(df$datetime)

pl2 <- ggplot(df,aes(x=datetime,y=count)) + geom_point(alpha = 0.4,aes(color=temp))+ theme_bw()
pl2 <- pl2 + scale_color_gradient(low='green',high ='red')
#print(pl2)

## FEATURE ENGINEERING 

df$HOUR <- sapply(df$datetime,function(x){format(x,"%H")})

# change hour to numeric
df$HOUR <- sapply(df$HOUR, as.numeric)


pl3 <- ggplot(filter(df,workingday==1),aes(x=HOUR,y=count)) + geom_point(alpha = 0.4,aes(color=temp))+ theme_bw()
pl3 <- pl3 + scale_color_gradientn(colors = c('dark blue','light blue','yellow','orange','red'))
print(pl3)

######## now build models

#  temp.model 

temp.model <- lm(count ~ temp, data = df)
#print(temp.model)

# at 25 degree celsius we will have   6.046 + 9.171(25) = 235
# anoher way is to do with predict 
temp.test <- data.frame(temp= c(25))
ans <- predict(temp.model, temp.test)
print(ans)
