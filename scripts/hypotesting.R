library(tidyverse)
data <- read.csv("data/insurance.csv")#import data

charges_vs_age_plot <- ggplot(data, aes(age, charges)) + # look if there correlation between charges and age
  geom_point() +
  labs(x = "Age", y = "Charges") + 
  ggtitle("Charges vs. Age")

charges_vs_age_sex_plot <- ggplot(data, aes(age, charges)) + # is sex makes difference?
  geom_point() +
  labs(x = "Age", y = "Charges") +
  ggtitle("Charges vs. Age by Sex") +
  scale_color_manual(values = c("red", "blue")) +  
  aes(color = sex)

data_c <- data %>% 
  filter(sex == "female") %>% 
  mutate(Group = ifelse(age >= 35, "Above 35", "Below 35")) #create a new data fr
#with females filtered by age
#in the new column
data_c %>% 
  count(Group) #women in each group

fig<- data_c %>% 
  ggplot(aes(x = Group, y = charges, color = Group)) +
  geom_boxplot() +
  labs(x = NULL, y = "Charges ($)", color = "Age") +
  theme_bw() #visualisation
ggsave(path = "figs", filename = "Charges by age group box plot.png") #save in the figs folder

t.test(charges ~ Group, data = data_c, alternative = "greater")#t-test(check if 
#we reject H0 hypothesis)