#dataset Titanic_train is loaded
data("titanic")
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

?titanic_train
head(titanic)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

#a plot of theoretical data with mean and Sd created using geom_qq against the given data using line
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params) + geom_abline()

#using bar chart to understand the amount of people who survived and the distribution of male and female
#between the people
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Sex)) + geom_bar(position = position_dodge())

#density chart of people who survived and did not survive according to Age
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha=0.2) 

#Boxplot showing the survivers and dead based on the Fare they paid
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot() 

#using density plot to differenciate between the class of people to see who survived and died more and seeing the age group
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha=0.2) + facet_grid(Sex~Pclass)

#3 different bar plots to see the amount of survived and dead in different classes chosen by people
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar()
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived,fill=Pclass)) +geom_bar(position = position_fill())
