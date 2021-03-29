# Problem Set 7

library(mice)
library(modelsummary)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(naniar)
library(tidyr)
library(VIM)





#4. To read in the data as a dataframe and taking the first row as column header 
wages<-read.csv("C:/Users/walee/OneDrive - University of Oklahoma/OU/PhD (Economics)/Semester 4/Data Science for Economists/DScourseS21/ProblemSets/PS7/wages.csv", header=TRUE) 


# Just to check values of the first 10 observations
head(wages,10)

#5. Dropping observations where either hgc or tenure are missing

wages <- wages %>% drop_na(hgc,tenure)



# Summary table of the dataframe using modelsummary:
# datasummary_skim(wages,histogram=F,output="markdown")
datasummary_skim(wages,histogram=F,output="latex")
# datasummary_skim(wages,histogram=F,output="myfile.tex")

# following gives an ideae of how many missing values are there for logwage
aggr(wages, prop=FALSE, numbers=TRUE)



# Conducting the following regression automatically performs for only complete cases as a cursory look from the results show
est1 <- lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married) , data=wages) 


#Mean imputation
wages <- wages %>% mutate(logwage2 = case_when(!is.na(logwage) ~ logwage, is.na(logwage) ~ mean(wages$logwage,na.rm=T)))

# Regression Estimate 2
est2 <- lm(logwage2 ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married) , data=wages)


# imputing missing log wages as their predicted values from the complete cases regression
pred.data = predict(est2, newdata=wages)
head(pred.data)

wages <- wages %>% mutate(logwage3 = case_when(!is.na(logwage) ~ logwage, is.na(logwage) ~ pred.data))

est3 <- lm(logwage3 ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married) , data=wages)


# Multiple regression imputation using mice

dat_imputed <- mice(wages, m = 10, printFlag = FALSE, seed = 12345, meth="norm.boot")

fit_mice <- with(dat_imputed, lm(logwage ~ hgc + as.factor(college) + poly(tenure,2,raw=T) + age + as.factor(married)))

mice_imputation <- mice::pool(fit_mice)


#Model Summary
modelsummary(list(est1, est2, est3, mice_imputation), output="latex")







  
  

