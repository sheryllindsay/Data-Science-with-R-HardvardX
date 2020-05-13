#loading libraries
library(dslabs)
data(heights)
options(digits = 3) 
library(dplyr)


#finding average mean values by getting logical values vectors
str(heights)
avg<-mean(heights$height)
avg
ind<-heights$sex=="Female"
ind
mean(ind)
min(heights$height)

#finding index values of variables in dataset using WITCH and MATCH
y<-which(heights$height==50)
x <- match(min(heights$height),heights$height)
y
heights$sex[1032]

#minimum and maximum
max(heights$height)
min(heights$height)

x<- 50:82
x

y<- !x %in% heights$height
sum(y)

#using MUTATE to create a new column with cm and get the mean
heights2<-mutate(heights,cm=height*2.54)
str(heights2)
heights2$cm[18]
mean(heights2$cm)

#using FILTER to get particular values from the dataset
females<-filter(heights2, sex=="Female")
sum(females)
sum(heights2$sex=="Female")

mean(females$cm)

#loading the olive dataset in dslaba
library(dslabs)
data(olive)
head(olive)

#the very basic plots: LINE,HISTOGRAM and BOXPLOT
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)

# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))

length(heights$sex)
238/1050*100

# define range of values spanning the dataset
#creating a cummilative distribution function
a <- seq(min(heights$height), max(heights$height), length = 100)
cdf_function <- function(x) { 
  print(x)# computes prob. for a single value
  mean(heights$height <= x)
} 
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)