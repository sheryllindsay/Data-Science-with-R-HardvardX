#loading the Titanic_train dataset
data("titanic")
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

#selecting the necessary data from the titanic_train dataset
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#gives information on the dataset
?titanic_train

#geom_density plot of people with respect to age with Sex classification
titanic %>% ggplot(aes(Age,color=Sex)) + geom_density()

sum(titanic$Sex=="male" & titanic$Age%in%17)/sum(titanic$Sex=="male" )
sum(titanic$Sex=="female" & titanic$Age%in%17)/sum(titanic$Sex=="female" )

#Sex of the passanger with maximum Age
t<-titanic %>% filter(!is.na(Age)) 
max(t$Age)
index<-which.max(t$Age)
t$Sex[index]
