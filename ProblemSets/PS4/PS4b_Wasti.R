library(sparklyr)
library(tidyverse)
library(dplyr)

#Setting up connection to Spark
spark_install(version = "3.0.0")
sc <- spark_connect(master = "local")

# Creating a table called df1
df1 <- as_tibble(iris)

##Copying the table into Spark
df <- copy_to(sc,df1)

#Verifying the types of the two dataframes
class(df1)
class(df)

#RDD/SQL operation: select
#Listing the first 6 rows of the Sepal_Length and Species columns of df
df %>% select(Sepal_Length,Species) %>% head %>% print

# RDD operation: filter
#Listing the first 6 rows of all columns of df where Sepal_Length is larger than 5.5
df %>% filter(Sepal_Length>5.5) %>% head %>% print

# Combine the two previous exercises into one line using dplyr pipeline
df %>% select(Sepal_Length,Species) %>% filter(Sepal_Length>5.5) %>% head %>% print


# RDD operation: group-by
# Computing the average sepal length, as well as the number of observations, by each of the three iris species: 
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print

# RDD operation: sort
# re-executing the previous call
df2 <- df %>% group_by(Species) %>% summarize(mean = mean(Sepal_Length), count = n()) %>% head %>% print
# using the arrange() function to sort the result ascending by species name
df2 %>% arrange(Species) %>% head %>% print
