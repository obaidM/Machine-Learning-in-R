# note that it is seprated by semi colon and not comma. so I added the statement that made it happen.
df <- read.csv('student-mat.csv', sep = ';')
library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrgram)
library(corrplot)
library(caTools)
# check for any null values
null.results <- any(is.na(df)) 
# now grab only the numeric columns. So data from numeirc will be core.data
# use sapply to apply a function on a data set
num.cols <- sapply(df,is.numeric)
# now get your corelation data 
# we run correlation on df but only on its numeric data. not on boolean or text data. Only on numeric
cor.data <- cor(df[,num.cols])
# we can present correlation data with corrplot

#print(  corrplot(cor.data)         )
#print( corrgram(df))

## lets get rid of zero values or predictions. we don't need them
to_zero <- function(x){
  if(x<0)
  {return (0)}else{
    return (x)
  }
  
}

######## Lets start with the linear regression#############
set.seed(101)
sample <- sample.split(df$G3,SplitRatio = 0.7)  # we are splitting the dataset so that w ecan make train & test
train <- subset(df,sample == TRUE)
test <- subset(df,sample == FALSE)
model <- lm(G3 ~. , data = train)
#####################################

# now we will drawa histogram for the residuals. whats left. it is the difference between the actual - predict. we want as low as possible.

res <- residuals(model)
res <- as.data.frame(res)
pl <- ggplot(res,aes(res)) + geom_histogram(fill='green',alpha = 0.5,color = 'blue')

### now its time to predict the values of g3 with functions called predict

G3.pr <- predict(model,test) # your model and sample of the test you want. 
result <- cbind(G3.pr,test$G3)




colnames(result) <- c('predict','actual') # making a data frame with just actual and predict
result <- as.data.frame(result)

result$predict <- sapply(result$predict,to_zero)

pl <- ggplot(result,aes(result$predict)) + geom_histogram(fill='green',alpha = 0.5,color = 'blue')
print(pl)


#### MSE Mean square Error 

mse <- mean((result$actual-result$predict)^2)
print(mse)
#root mean square 
RMS <- mse^0.5
print(RMS)


## RSQUARE IS 1- SSE/SST
SSE <-  sum((result$actual-result$predict)^2) # Sum of square error 
SST <- sum( (mean(df$G3)- result$actual)^2  ) # sum of square total 
rsquare <- 1-(SSE/SST)

print(rsquare)



