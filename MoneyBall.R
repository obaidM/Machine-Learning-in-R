library(ggplot2)
library(dplyr)
library(plotly)
batting <- read.csv('Batting.csv')
batting <- subset(batting,yearID>=1985)
sal <- read.csv('Salaries.csv')

# add a column batting average
batting <- mutate(batting,BA = H/AB)
batting <- mutate(batting, OBP = (H+BB+HBP)/(AB+BB+HBP+SF)  )
batting <- mutate(batting, X1B = (H-X2B-X3B-HR) )
#slugging average
batting <- mutate(batting,SLG = (X1B+(2*X2B)+(3*X3B)+(4*HR))/AB)
# combine sal & batting average
combo = merge(batting,sal,by=c('playerID','yearID'))
combo <- subset(combo,yearID==2001)
# oakland lost 3 players. we need to get there info to understand what exactly we ahve lost and how to replace it.
lost.players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
lost.players <- subset(lost.players,yearID==2001)
lost.players <- lost.players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB','salary')]

# now we need to replace the 3 players
# amount of salary should not exceed $15MM 
# AB sum should be 1469 
# AVG  0.364 OBP


#print(pl)

combo <- combo %>% filter(OBP>0.45, OBP<0.99,salary<8000000)
combo <- select(combo,playerID,BA,OBP,salary,SLG,teamID.y)
arrange(combo,(BA),desc(salary))


pl <- ggplot(combo,aes(x=OBP,y=salary))
pl <- pl + geom_point(size=3)
pl<- ggplotly(pl)
print(pl)
print(combo)