#install.packages("rio")
library("rio")
hinsu <- rio::import("data/insurance.csv")
View(hinsu)
str(hinsu)

library(ggplot2)
library(dplyr)

### Exploring charges
summary(hinsu$charges) 
 
### Exploring the age ###
sort(unique(hinsu$age)) 
range(hinsu$age) # the range is between 18 and 64
summary(hinsu$age) # descriptive stats
ggplot(hinsu, aes(age)) +
  geom_histogram(binwidth = 1) # plotting to see the distribution to decide 
                               # whether to use mean or median, distribution is more like uniform

