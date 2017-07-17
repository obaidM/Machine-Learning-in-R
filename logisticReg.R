df.train <- read.csv('titanic_train.csv')
### EXPLORATORY DATA ANALYSIS
# we will install amelia package to see who is missing and who is not.
library(Amelia)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(caTools)
########################
impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    if(is.na(age[i])){
      
      if(class[i]==1){out[i] <- 37}
      if(class[i]==2){out[i]<-29}
      if(class[i]==3){out[i]<-24}
      
    }else{out[i]<- age[i]}
  }
    return(out)
  }



#####################################################

missmap(df.train, main = 'Missing Map', col= c('yellow','black'),legend = FALSE)
#print(missmap())

#pl <- ggplot(df.train,aes(Survived)) + geom_bar(position='dodge') + theme_bw()

#pl <- ggplot(df.train,aes(Pclass)) + geom_bar(aes(fill=factor(Pclass))) + theme_bw()

#pl <- ggplot(df.train,aes(Sex)) + geom_bar(aes(fill=factor(Sex))) + theme_bw()

#pl <- ggplot(df.train,aes(Age)) + geom_histogram(aes(fill=factor(Sex)),alpha =0.5,fill = 'green') + theme_bw()

pl <- ggplot(df.train,aes(Fare)) + geom_histogram(color= 'green',alpha =0.5,fill = 'blue') + theme_bw()




########## Imputing function called
fixed.age <- impute_age(df.train$Age,df.train$Pclass)

df.train$Age <- fixed.age

################## Now some feature Enngineering , lets fill those 177 rows with no age
# first a box plot to see how is the age factored in 
pl <- ggplot(df.train,aes(x=factor(Pclass),y=Age))
pl<- pl + geom_boxplot(aes(group=Pclass,fill=factor(Pclass)),alpha=0.4) + theme_bw()
pl <- pl + scale_y_continuous(breaks = seq(min(0),max(80),by=2))
pl <- ggplotly(pl)
print(pl)
############ removing colummns. Sometimes you don't need extra data . so delete them
df.train <- select(df.train,-PassengerId,-Name,-Cabin,-Ticket)

######## lets start modelling
log.model <- glm(Survived ~. , family = binomial(link = 'logit'),data = df.train)


## now lets start with the predict 
set.seed(101)
split <- sample.split(df.train$Survived,SplitRatio = 0.7)
final.train <- subset(df.train,split == TRUE)
final.test <- subset(df.train,split == FALSE)

final.log.model <- glm(Survived ~. , family = binomial(link = 'logit'),data = final.train)
print(summary(final.log.model))
## NOW ACTUAL PREDICT on test model 

fitted.prob <- predict(final.log.model,fInal.test,class='response')
results <- ifelse(fitted.prob>0.5,1,0)
misclassErr <- mean(results != final.test$Survived)
accuracy <- 1-misclassErr
print(accuracy)
### lets create confusion matrix . we use table function 
cm <- table(final.test$Survived,fitted.prob>0.5)
print(cm)


