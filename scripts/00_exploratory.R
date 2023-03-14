#install.packages("rio")
#install.packages("MASS")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("Hmisc")
#install.packages("broom")
#install.packages("ggpubr")
#install.packages("corrplot")

rm(list = ls())  #clearing environment

### Importing and viewing data ### ----
library(rio)
df <- rio::import("data/insurance.csv") 
View(df)
str(df)

library(ggplot2)
library(tidyverse)

### Exploring charges ### ----
summary(df$charges) # assuming 1000 USD/year
range(df$charges) # the range is between 1121.874 and 63770.428

plot(density(df$charges))

library("car")
qqPlot(df$charges)
ggplot(df, aes(charges)) +
  geom_histogram(binwidth = 1000) # plotting to see the distribution 
                                  # like 


### Exploring the age ### ----
sort(unique(df$age)) 
range(df$age) # the range is between 18 and 64
summary(df$age) # descriptive stats
ggplot(df, aes(age)) +
  geom_histogram(binwidth = 1) # plotting to see the distribution to decide 
                               # whether to use mean or median, distribution is more like uniform

### Exploring the sex ### ----
sort(unique(df$sex))
df$sex <- factor(df$sex)
summary(df$sex)
plot(df$sex)

### Smoker ### ----
sort(unique(df$smoker))
df$smoker <- factor(df$smoker)
summary(df$smoker)
plot(df$smoker)

#general correlation between numeric variables ----
library(Hmisc)
library(broom)
library(ggpubr)
library(corrplot)

cor_df <- df[,c(1,3,4,7)] %>% 
  cor()

df[,c(1,3,4,7)] %>% 
  as.matrix() %>% 
  rcorr()           #P-values small: children~charges

corrplot(cor_df, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)  #The stronger the color and the bigger the size, 
                                         #the higher the correlation ----->?? these are not p-values
#https://towardsdatascience.com/exploratory-data-analysis-in-r-for-beginners-fe031add7072

### Female over 35 ### ----
f35 <- filter(df,age >= '35' & sex == 'female')
summary(f35)
range(f35$age)

#correlation between numeric variables, females over 35
cor_f35 <- f35[,c(1,3,4,7)] %>% 
  cor()                 

f35[,c(1,3,4,7)] %>% 
  as.matrix() %>% 
  rcorr()             #P-values small: bmi~charges


corrplot(cor_f35, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)  #The stronger the color and the bigger the size, 
                                          #the higher the correlation.

### Male over 35 ### ----
m35 <- filter(df,age >= '35' & sex == 'male')
summary(f35)
range(f35$age)

#correlation between numeric variables, males over 35
cor_m35 <- m35[,c(1,3,4,7)] %>% 
  cor()

m35[,c(1,3,4,7)] %>% 
  as.matrix() %>% 
  rcorr()             #P-values small: age~bmi, age~charges, bmi~charges

#corrplot

corrplot(cor_m35, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)  #The stronger the color and the bigger the size, 
                                        #the higher the correlation.
