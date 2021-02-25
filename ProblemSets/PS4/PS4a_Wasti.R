library(tidyverse)
library(jsonlite)

#Loading the file
system('wget -O dates.json "https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20210219&lang=en"')

#Printing the file
cat dates.json

# Converting the file to a dataframe
json_file <- "dates.json"
mylist <- fromJSON(’dates.json’)
mydf <- bind_rows(mylist$result[-1])

# What type of object mydf is?
class(mydf)

# What type of object mydf$date is?
class(mydf$date)

# Printing the first n rows where n=10 
head(mydf,10)
