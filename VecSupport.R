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

#### install.packages("e1071")
loan <- read.csv("loan_data.csv")
### so many of them should be factors not integers  
# lets clean data 

loan$credit.policy <- factor(loan$credit.policy)
loan$inq.last.6mths <- factor(loan$inq.last.6mths)
loan$delinq.2yrs <- factor(loan$delinq.2yrs)
loan$pub.rec <- factor(loan$pub.rec)
loan$not.fully.paid <- factor(loan$not.fully.paid)
######## some EDA 

# first plot 

pl <- ggplot(loan, aes(fico))
pl <- pl + geom_histogram(aes(fill=not.fully.paid),color='black',bins=40,alpha =0.6 ) + theme_bw()
pl <- pl + scale_fill_manual(values = c('green','orange')) # how to change default colors
print(pl)


# second plot 
pl <- ggplot(loan, aes(factor(purpose)))
pl <- pl + geom_bar(aes(fill=not.fully.paid),alpha =0.6, position = 'dodge' ) + theme_bw()
pl <- pl + scale_fill_manual(values = c('green','orange')) # how to change default colors
print(pl)

# third plot 

pl <- ggplot(loan, aes(int.rate,fico) )
pl <- pl + geom_point(aes(color=not.fully.paid),alpha =0.5 ) + theme_bw()
pl <- pl + scale_color_manual(values = c('green','red'))
print(pl)

########
# lets build a model by splitting train and test. 
set.seed(101)
split <- sample.split(loan$not.fully.paid,SplitRatio = 0.7)
train <- subset(loan,split == TRUE)
test <- subset(loan,split == FALSE)

# lets train our SVM model 
model <- svm(not.fully.paid ~.,train)
####### go to predict and check out your accuracy in the 

# pred.val <- predict(model,test[1:13])
# table(pred.val,test$not.fully.paid)  ## result came out to be really bad. Must tune the model
# we are using a bad cost and gamma value 

tuned.results <- tune(svm,train.x = not.fully.paid ~. , data = train, kernel="radial",ranges = list(cost=c(100:130),gamma=c(0.1))

print(summary(tuned.results))





