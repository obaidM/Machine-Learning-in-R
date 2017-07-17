#install.packages('dplyr')
#install.packages('nycflights13')
library(dplyr)
library('nycflights13')

# select selects the columns for me 

df <- select(head(flights),carrier,tailnum)

print(df)

# distinct. only the distinct values

print(distinct (flights, carrier))

# mutate adds colums to data sets 

my.data <- select(head(flights), carrier,tailnum, arr_time)
my.data <- mutate(my.data, obs_col = arr_time/2)
print(my.data)

# summarize is beautiful. also na.rm means get rid of N/A values.
ans <- summarize(flights,avg_air_time=mean(air_time,na.rm = TRUE))
print(ans)

ans2 <- summarize(flights,tots=sum(dep_delay,na.rm = TRUE))
print(ans2)