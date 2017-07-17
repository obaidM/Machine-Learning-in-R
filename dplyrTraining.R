library(dplyr)
df <- mtcars 

#q1
q1 <- df %>% filter(mpg>20,cyl==6)

print(q1)
#q2  so simple yet such a strong statment
q2 <- arrange(df,cyl,desc(wt))

print(q2)

#q3
q3 <- select(df,hp,mpg)
print(q3)

#q4 

q4 <- distinct(df,gear)
print(q4)

#q5
q5 <- mutate(df,performance = hp/wt)
print(q5)

#q6 

q6 <- summarize(df,mean(mpg,na.rm=TRUE))
print(q6)

#q7

q7 <- df %>% filter(cyl==6)%>% summarize(mean(hp))
print(q7)