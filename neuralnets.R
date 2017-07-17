########### NEURAL NETS ###########3
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
library(neuralnet)


library(MASS)
#########3
# we will try to find median value thru neural network reinforcment learning 
## check for missing data 
df.original <- Boston
## no missing data 

## for neural networks always normalize your data 
##### get max and min vales of every column by using apply 
max.values <- apply(df.original,2,max)
min.values <- apply(df.original,2,min)
nor.df <- scale(df.original,center=min.values,scale = max.values-min.values)   ### normalized data 
df <- as.data.frame(nor.df)

###### Lets split data into train and tets sample 
split <- sample.split(df$medv,SplitRatio = 0.7)
train <- subset(df,split == T)
test <- subset(df, split == F)

####### we are ready to cal nueral Network function on our train model first

nn <- neuralnet(medv ~ crim + zn + indus + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data=train,hidden=c(5,3),linear.output = T  )
plot(nn)

### lets test to predict some values 

predicted.nn.values <- compute(nn,test[1:13])       ### WE LEAVE ONE OUT since we are trying to predict the final column

### computed result has a column net.result --- predicted.nn.values$net.result
## REVERSE scale to birng them back to the original value
true.predicted <- predicted.nn.values$net.result *(max(df.original$medv)-min(df.original$medv))+ min(df.original$medv) 

test.r <- test$medv *(max(df.original$medv)-min(df.original$medv))+ min(df.original$medv)

## lets check our MSE mean square error 

MSE.nn <- sum((test.r-true.predicted)^2)/nrow(test)

error.df <- data.frame(test.r,true.predicted)

##### lets draw our errors on a scatter plot 

pl <- ggplot(error.df,aes(x=test.r,y=true.predicted)) + geom_point(alpha=0.5,color="red")+ stat_smooth()
##print(pl)


