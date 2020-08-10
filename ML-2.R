#Section 2: Machine Learning Basics
library(dslabs)
library(dplyr)
library(lubridate)
library(tidyverse)
install.packages("caret")
library(caret)
#2.1 Basics of Machine Learning algorithms

#mid-test
#2
x<-read_mnist()
mnist <- read_mnist()
ncol(mnist$train$images)

#assesment
data(reported_heights)
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

#1
fic_p<-sum(dat$sex=="Female" & dat$type=="inclass")/sum(dat$type=="inclass")
fic_p
fo_p<-sum(dat$sex=="Female" & dat$type=="online")/sum(dat$type=="online")
fo_p

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

#2
N<-length(x)

guess<-sample(c("Male","Female"),N,replace=TRUE,prob=c(0.34,0.66))
dat<- dat %>% mutate(Y_hat=guess)
head(dat)

cutoff<-c("online")
accuracy<- map_dbl(cutoff,function(x){
  Y_hat<-ifelse(dat$type==cutoff,"Male","Female")
  mean(Y_hat==dat$sex)
})
accuracy

sum(dat$sex=="Male" & dat$type=="inclass")/sum(dat$type=="inclass")
sum(dat$sex=="Male" & dat$type=="online")/sum(dat$type=="online")

y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)

#3
conf_m<-table(y_hat,y)

#4
sensitivity(conf_m)

#5
specificity(conf_m)

#6
sum(y=="Female")/length(y)

#assesment part 2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#7
set.seed(2, sample.kind="Rounding")
# line of code
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

#8
head(train)
levels(train$Species)
vir<-which(train$Species=="virginica")
ver<-which(train$Species=="versicolor")
mean(train$Sepal.Length[ver])

#Sepal.Length
cutoff<-seq(min(train$Sepal.Length),max(train$Sepal.Length),0.1)
foo1<-function(x){
  Y_hat<-ifelse(train$Sepal.Length>x,'virginica','versicolor') 
  mean(Y_hat==train$Species)
}
accuracy<- sapply(cutoff,foo1)
max(accuracy)
#Sepal.Width
cutoff<-seq(min(train$Sepal.Width),max(train$Sepal.Width),0.1)
accuracy<- sapply(cutoff,function(x){
  Y_hat<-ifelse(train$Sepal.Width>x,"virginica","versicolor") 
  mean(Y_hat==train$Species)
})
max(accuracy)
#Petal.Length
cutoff<-seq(min(train$Petal.Length),max(train$Petal.Length),0.1)
accuracy<- map_dbl(cutoff,function(x){
  Y_hat<-ifelse(train$Petal.Length>x,"virginica","versicolor") %>% factor(levels = levels(train$Species))
  mean(Y_hat==train$Species)
})
max(accuracy)
#Petal.Width
cutoff<-seq(min(train$Petal.Width),max(train$Petal.Width),0.1)
cutoff<-seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
accuracy<- sapply(cutoff,function(x){
  Y_hat<-ifelse(train$Petal.Width>x,"virginica","versicolor")
  mean(Y_hat==train$Species)
})
max(accuracy)

#easy code
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)

#9
#feature selected is Petal.Length with 0.96
foo2<-function(x){
  Y_hat<-ifelse(test$Petal.Length>x,"virginica","versicolor")
  mean(Y_hat==train$Species)
}
sapply(cutoff[which.max(accuracy)],foo2)

#10
foo3 <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions_test <- sapply(test[,-c(1,2,5)],foo3)
sapply(predictions_test,max)

#11
plot(iris,pch=21,bg=iris$Species)
cutoff1<-seq(min(test$Petal.Width),max(test$Petal.Width),0.1)
cutoff2<-seq(min(test$Petal.Length),max(test$Petal.Length),0.1)

foo3 <- function(x){
  sapply(cutoff2,function(i){
      y_hat <- ifelse(test$Petal.Width>x & test$Petal.Length>i,'virginica','versicolor')
      mean(y_hat==test$Species)
  })
}
predictions_test <- sapply(cutoff1,foo3)
max(predictions_test)

#ans
petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)

length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7

width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5

y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)
#-------------------------------------------------------------------------------------

#2.2 Conditional Probabilities

#assessment

#1
#                P(test+|sick)P(sick)
#P(sick|test+) = --------------------
#                   P(test+)

#P(test+) = P(test+|sick) * P(sick) + P(test+|healthy) * P(healthy)
P_test<-(0.1*0.98)+(0.85*0.02)
P_sick_ptest<-0.85*0.02/0.115

#2-5
set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

#2
mean(test)

#3
mean(disease[test==0])

#4
mean(test[disease==1])*mean(disease)/mean(test)
mean(disease[test==1]==1)

#5
mean(disease[test==1]==1)/mean(disease)

#6
data("heights")
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%qplot(height, p, data =.)

#7
ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

#8
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
plot(dat)

ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

#----------------------------------------------------------------------------
