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

df <- College

joiner <- function(x){
  if(x >=0.5){return('yes')}
  if(x<0.5){return('no')}
}
######## TREE METHOD PROJECT , WE WILL TRY to predict if the college is private or not
 # FEW plots 

pl <- ggplot(df, aes(Room.Board,Grad.Rate))+ geom_point(aes(color=Private),alpha=0.5,size=4)

pl <- ggplot(df, aes(F.Undergrad))+ geom_histogram(aes(fill=Private),alpha=0.5,color='black', bins=50) +theme_bw()



## one of the college has a grad rate of 118%. that is not possible. so lets change it to 100
df['Cazenovia College','Grad.Rate'] <- 100

pl <- ggplot(df, aes(Grad.Rate))+ geom_histogram(aes(fill=Private),alpha=0.5,color='black', bins=50) +theme_bw()
#print(pl)

##### split and train the data
set.seed(101)
split <- sample.split(df$Private,SplitRatio = 0.7)
train <- subset(df,split == TRUE)
test <- subset(df,split == FALSE)

####### 
## LETS BUILD A DECISION TREE
tree <- rpart(Private ~., method='class',data=train)
tree.pred <- predict(tree,test)
# lets turn this into a data frame
tree.pred <- as.data.frame(tree.pred)

tree.pred$private <- sapply(tree.pred$Yes,joiner)

ans <- prp(tree)

print(ans)

################################
## RANDOM FOREST
###############################

rf.model <- randomForest(Private ~., data=train, importance = TRUE)

rf.pred <- predict(rf.model,test)
 tb <- table(rf.pred,test$Private)
 
 print(tb)
