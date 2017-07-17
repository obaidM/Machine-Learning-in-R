library(ISLR)
library(Amelia)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(caTools)
library(class)
df <- iris 


# before you perform k nearest neighbor.
#Please standardize  the columns so there are not a lot of different y values. Except the value you trying to predict 
###  standardize the data 
stnd.data <- scale(df[1:4])
df <- cbind(stnd.data,df[5])
# in console check the variance 

###### lets split the data 
set.seed(101)
sample <- sample.split(df$Species,SplitRatio = 0.7)

train <- subset(df, sample == TRUE)
test <- subset(df, sample == FALSE)

######### 
# KNN MODEL 
#############

pred.Species <- knn(train[1:4],test[1:4], train$Species,k=3)

err <- mean(test$Species != pred.Species)
print(err)

##### lets find all the potential values thru a for  loop

for (i in 1:10){
  set.seed(101)
  pred.Species <- knn(train[1:4],test[1:4], train$Species,k=i)
  err[i] <- mean(test$Species != pred.Species)
  }
## plot out the reults 
# make a quick data frame for this plot 

k.val <- 1:10 
err.val <- data.frame(err,k.val)

pl <- ggplot(err.val,aes(x=k.val,y= err))+geom_point()
pl <- pl + geom_line(lty='dotted', color = 'red')

print(pl)

# optimal k =3 


