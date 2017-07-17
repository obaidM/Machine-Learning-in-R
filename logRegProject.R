adult <- read.csv('adult_sal.csv')

library(Amelia)
library(ggplot2)
library(ggthemes)
library(plotly)
library(dplyr)
library(caTools)
## drop index. it has been repeated 
adult <- select(adult,-X)



######## Data Cleaning############################
# 1 make unemp category
unemp <- function(job){
  job <- as.character(job)
  if(job =='Without-pay' | job == 'Never-worked'){
    return ('unemp')
  }else{return(job)}
}
## lets apply the function on adult 
adult$type_employer <- sapply(adult$type_employer,unemp)

# 1 make state and local giv category 
SL.gov <- function(job){
  job <- as.character(job)
  if(job =='Local-gov' | job == 'State-gov'){
    return ('SL.gov')
  }else{return(job)}
}
## lets apply the function on adult 
adult$type_employer <- sapply(adult$type_employer,SL.gov)


# make state and local giv category 
Self.emp <- function(job){
  job <- as.character(job)
  if(job =='Self-emp-inc' | job == 'Self-emp-not-inc'){
    return ('Self.emp')
  }else{return(job)}
}
## lets apply the function on adult 
adult$type_employer <- sapply(adult$type_employer,Self.emp)

## lets clean the marital status column 
marital_status <- function(marital){
  marital <- as.character(marital)
  if(marital == 'Married-AF-spouse'| marital =='Married-civ-spouse'| marital == 'Married-spouse-absent' ){return('Married')}
  if(marital == 'Divorced' | marital == 'Separated'| marital == 'Widowed'){return('Not-Married')}
  if(marital=='Never-married'){return('Never-married')}
}

adult$marital <- sapply(adult$marital,marital_status)
#### lets group these countries to clean up the data.
Asia <- c('China','Hong',' India','Iran','Camodia','Japan','Laos','Philippines','Vietnam','Taiwan','Thailand')
North.Amer <- c('Canada','United-States','Puerto-Rico')
Europe <- c('England','France','Germany','Greece','Holland-Netherlands','Hungary','Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')
Latin.Amer <- c('Columbia','Cuba','Dominican-Republic','Ecuador','El-Savodor','Guatmala','Haiti','Honduras','Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru','Jamaica','Trinadad&Tobago ')


country.grouping <- function(country){
  if(country %in% Asia){return('Asia')}
  if(country %in% North.Amer){return('North.Amer')}
  if(country %in% Europe){return('Europe')}
  if(country %in% Latin.Amer){return('Latin.Amer')}
 if((country %in% c(Asia,North.Amer,Europe,Latin.Amer)== F)){return('Other')}
}
adult$country <- sapply(adult$country,country.grouping)
########### Very important. apply factor on these updated columns. Since they are character field.

### Time to get rid of "?" to N/A 
adult[adult == '?'] <- NA  # such a great way to convert things to NA. Now we can do mismap and other manipulation 

adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)
adult$type_employer <- sapply(adult$type_employer,factor)
########### lets start with missmap from amelia package 
hmap <- missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

# lets drop the NA data 

adult <- na.omit(adult)

hmap <- missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))


## rename the column country to Region
adult <- rename(adult,region=country)
#######################
###### DATA VISUALIZATION // EDA 

pl <- ggplot(adult,aes(age)) + geom_histogram(aes(fill=income),alpha = 0.5,color='black',binwidth = 1) + theme_bw()
pl <- ggplotly(pl)
#print(pl)
## hours worked per week

pl <- ggplot(adult,aes(hr_per_week)) + geom_histogram(aes(fill=income),alpha = 0.5,color='black',binwidth = 1) + theme_bw()
pl <- ggplotly(pl)

### region by salary 
pl <- ggplot(adult,aes(region)) + geom_bar(aes(fill=income),alpha = 0.5,color='black') + theme_bw()
pl <- ggplotly(pl)
#print(pl)

## eductaion 

pl <- ggplot(adult,aes(education)) + geom_bar(aes(fill=income),alpha = 0.5,color='black') + theme_bw()
pl <- ggplotly(pl)
print(pl)

# occupation 

pl <- ggplot(adult,aes(occupation)) + geom_bar(aes(fill=income),alpha = 0.5,color='black') + theme_bw()
pl <- ggplotly(pl)
#print(pl)

# by type_employer

pl <- ggplot(adult,aes(type_employer)) + geom_bar(aes(fill=income),alpha = 0.5,color='black') + theme_bw()
pl <- ggplotly(pl)
#print(pl)

###############################################
###Logistic Regression########################
###############################################


set.seed(101)
split <- sample.split(adult$income,SplitRatio = 0.7)
train <- subset(adult,split == TRUE)
test <- subset(adult,split == FALSE)
# logistic regression model on train set 
model <- glm(income ~. , family = binomial(link = 'logit'),data = train)
# we used step to get rid of fields that are not important. there are many models for this step.
new.step.model <- step(model)
# make your predicted column in test sample. Now we can compare it with the real one.
test$predicted.income <- predict(model,newdata = test,type = 'response')

# now lets do our confusion matrix
cm <- table(test$income,test$predicted.income >0.5)
print(cm)

# find accuracy   right/total from cm 
# find recall     right of false set (not greater than 0.5) / total of that set 
# find precision    right of false set ( not greater than 0.5)/total of the false set
acc <- (6372 + 1424)/(6372+1424+ 871+548)
recall <- (6372)/(6372+548)
precision <- (6372)/(6372+871)
