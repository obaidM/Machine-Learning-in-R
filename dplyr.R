# install package (dplyr)
#install package (nycflights13)


library(dplyr)
library('nycflights13')

df <- filter(flights, month==1,  carrier == 'UA', day == 3, dep_delay<2, arr_delay<3)
print(df)
# arrange  & slice are also two other functions you can use. 
#for select and other features please see dplyr2