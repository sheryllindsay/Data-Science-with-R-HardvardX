a<-25
install.packages("dslabs")
install.packages("tidyverse")
install.packages("tidyverse")
library(tidyverse)
library(dslabs)
data(murders)
murders %%
  ggplot(aes(population,total,label=abb,color=region))+
  geom_label()
data()\
data()
library(tidyverse)
library(dslabs)
data("murders")
murders %%
  ggplot(aes(population,total,label=abb,color=region))+
  geom_label()
data("murders")
class(murders)
murders %%
  ggplot(aes(population,total,label=abb,color=region))+
  geom_label()
data("murders")
library(ggplot2)
murders %%
  ggplot2(aes(population,total,label=abb,color=region))+
  geom_label()
murders %%
  ggplot(aes(population,total,label=abb,color=region))+
  geom_label()
murders %%
  ggplot(aes(population,total,label=abb,color=region))
geom_label()
View(murders)
log4(1024)
log(1024,4)
library(dslabs)
data(movielens)
head(movielens)
length(movielens)
length(movielens$movieId)
class(movielens$title)
class(movielens$genres)
levels(movielens$genres)
nlevels(movielens$genres)
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
x<-time/60
x
0.8*6
speed<-distaance*60/time
speed
speed<-distance*60/time
speed
library(dslabs)
data(heights)
options(digits = 3)
str(hwight)
str(height)
str(heights)
avg(heights$height)
avg<-mean(heights$height)
avg
ind<-which(heights$height>avg)
ind
sum(ind)
sum(heights$height)
ind<-heights$height>avg
ind
sum(ind)
View(heights)
ind<-heights$height>avg & heights$sex=="Female"
ind
sum(ind)
ind<-heights$sex=="Female"
ind
mean(ind)
min(heights$height)
x <- match (min(heights$height),heights$height)
x
heights[1034]
heights$height[1034]
heights$height[1032]
y<-which(heights$height==50)
y
heights$sex[1032]
max(heights$height)
min(heights$height)
x<- 50:82
x
y<- which(x%in%heights$height)
y
y<- x%in%heights$height
y
y<- !x %in% heights$height
y
sum(y)
heights2<-mutate(heights,cm=height*2.54)
library(dplyr)
heights2<-mutate(heights,cm=height*2.54)
str(heights2)
heigths2$cm[18]
heights2$cm[18]
mean(heights2$cm)
females<-filter(heights2, sex="Female")
females<-filter(heights2, sex=="Female")
sum(females)
sum(heights2$sex=="Female")
mean(females$height)
mean(females$cm)
library(dslabs)
data(olive)
head(olive)
plot(olive$palmitic,olive$palmitoleic)
hist(eicosenoic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)
boxplot(region~palmitic,data=olive)
boxplot(region~palmitic,data=olive)
boxplot(palmitic~region,data=olive)
library(dslabs)
data(heights)
Use summary() on the heights$height variable to find the quartiles:
  summary(heights$height)
Find the percentiles of heights$height:
  p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)
Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles. Note that quantile() returns a named vector. You can access the 25th and 75th percentiles like this (adapt the code for other percentile values):
  percentiles
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
names(percentile)
names(percentiles)
x<-quantile(heights$height,25)
x
x<-quantile(heights$height,0.25)
x
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
z
observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)
observed_quantiles
theoretical_quantiles
library(tidyverse)
library(dslabs)
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")
library(tidyverse)
library(dslabs)
data(gapminder)
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)
dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE)
# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))
# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")
# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50")
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0))
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt")
# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue")
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal()
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue")
# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank())
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")
# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",
            show.legend = FALSE, alpha = 0.2, size = 1)
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black")
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50",
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")
data("Titanic")
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
data("titanic")
install.packages("titanic")
options(digits = 3)    # report 3 significant digits
library(tidyverse)
data("titanic")
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic
?titanic_train
titanic %>% ggplot(aes(age,color=sex)) + geom_density()
?titanic_train
titanic %>% ggplot(aes(Age,color=Sex)) + geom_density()
titanic %>% ggplot(aes(Age,fill=Sex)) + geom_density()
titanic %>% ggplot(aes(Age,color=Sex)) + geom_density()
sum(titanic$sex=="female")
sum(titanic$Sex=="female")
sum(titanic$Sex=="male")
sum(titanic$Sex=="female" & age=40)
sum(titanic$Sex=="male" & age==40)
sum(titanic$Sex=="female" & Age==40)
?titanic_train
sum(titanic$Sex=="male" & Age %in% 40)
sum(titanic$Sex=="male" & titanic$Age %in% 40)
sum(titanic$Sex=="female" & titanic$Age %in% 40)
sum(titanic$Sex=="male" & titanic$Age %in% 18:35)/sum(titanic$Sex=="male" )
sum(titanic$Sex=="female" & titanic$Age %in% 18:35)/sum(titanic$Sex=="female" )
sum(titanic$Sex=="male" & titanic$Age<17)/sum(titanic$Sex=="male" )
sum(titanic$Sex=="female" & titanic$Age<17)/sum(titanic$Sex=="female" )
sum(titanic$Sex=="female" & titanic$Age%in%17)/sum(titanic$Sex=="female" )
sum(titanic$Sex=="male" & titanic$Age%in%17)/sum(titanic$Sex=="male" )
max(titanic$Age)
titanic %>% max(Age)
class(titanic%Age)
class(titanic$Age)
max(titanic$Age)
titanic %>% filter(!is.na(Age)) %>% max(titanic$Age)
titanic %>% filter(!is.na(Age))
t<-titanic %>% filter(!is.na(Age))
max(t$Age)
index<-which.max(t$Age)
t[index]
index
index<-which(max(t$Age))
.max
index<-which.max(t$Age)
index
titanic[index]
t[index]
t$Sex[index]
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(data=titanic)) + geom_qq(dparams=params)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(data=Age)) + geom_qq(dparams=params)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params) + abline()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(sample=Age)) + geom_qq(dparams=params) + geom_abline()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Age)) + geom_bar()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Sex)) + geom_bar()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Age)) + geom_bar()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_bar()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived),position = position_dodge()) + geom_bar()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_bar(position = position_dodge())
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age)) + geom_bar(position = position_dodge(aes(Survived)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived)) + geom_bar(position = position_dodge(aes(Survived)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived)) + geom_bar(position = position_dodge(aes(Age)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived)) + geom_bar(position = position_dodge(aes(Age)))
Age
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age)) + geom_bar(position = position_dodge(aes(Survived)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age)) + geom_bar(position = position_dodge(
)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age)) + geom_bar(position = position_dodge()))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age)) + geom_bar(position = position_dodge())
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age)) + geom_bar()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_bar(position = position_dodge())
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_bar(position = position_dodge(aes(Survived)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_bar(position = position_dodge(aes(Age)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Age)) + geom_bar(position = position_dodge(aes(Age)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Sex)) + geom_bar(position = position_dodge(aes(Age)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Sex)) + geom_bar(position = position_dodge(aes(Sex)))
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Survived,fill=Sex)) + geom_bar(position = position_dodge())
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density(alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,count,fill=Survived)) + geom_density(alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density(alpha=0.2) + scale_y_continuous(trans=count)
?titanic_train
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density(alpha=0.2) )
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived),alpha=0.2) + geom_density()
alpha=0.2
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density(alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density()
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density(alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,fill=Survived)) + geom_density(y=..count..,alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha=0.2)
titanic %>% filter(!is.na(Age)) %>% count(Age %in% 1:8)/count(Age %in% 1:80)
titanic %>% filter(!is.na(Age)) %>% count(titanic$Age %in% 1:8)/count(titanic$Age %in% 1:80)
count(titanic$Age %in% 1:8)/count(titanic$Age %in% 1:80)
titanic$Age %in% 1:8
sum(titanic$Age %in% 1:8)/sum(titanic$Age %in% 1:80)
sum(titanic$Age %in% 10:18)/sum(titanic$Age %in% 1:80)
sum(titanic$Age %in% 18:30)/sum(titanic$Age %in% 1:80)
sum(titanic$Age %in% 30:50)/sum(titanic$Age %in% 1:80)
sum(titanic$Age %in% 50:70)/sum(titanic$Age %in% 1:80)
sum(titanic$Age %in% 70:80)/sum(titanic$Age %in% 1:80)
head(titan)
head(titanic)
titanic1<- titanic %>% filter(Survived == 0)
sum(titanic1$Age %in% 1:8)/sum(titanic1$Age %in% 1:80)
sum(titanic1$Age %in% 10:18)/sum(titanic1$Age %in% 1:80)
sum(titanic1$Age %in% 18:30)/sum(titanic1$Age %in% 1:80)
sum(titanic1$Age %in% 30:50)/sum(titanic1$Age %in% 1:80)
sum(titanic1$Age %in% 50:70)/sum(titanic1$Age %in% 1:80)
sum(titanic1$Age %in% 70:80)/sum(titanic1$Age %in% 1:80)
head(titanic1)
sum(titanic1$Age %in% 1:8)/sum(titanic$Age %in% 1:80)
sum(titanic1$Age %in% 10:18)/sum(titanic$Age %in% 1:80)
sum(titanic1$Age %in% 18:30)/sum(titanic$Age %in% 1:80)
sum(titanic1$Age %in% 30:50)/sum(titanic$Age %in% 1:80)
sum(titanic1$Age %in% 50:70)/sum(titanic$Age %in% 1:80)
sum(titanic1$Age %in% 70:80)/sum(titanic$Age %in% 1:80)
sum(titanic1$Age %in% 1:8)/sum(titanic$Age %in% 1:8)
sum(titanic1$Age %in% 10:18)/sum(titanic$Age %in% 10:18)
sum(titanic1$Age %in% 18:30)/sum(titanic$Age %in% 18:30)
sum(titanic1$Age %in% 30:50)/sum(titanic$Age %in% 30:50)
sum(titanic1$Age %in% 50:70)/sum(titanic$Age %in% 50:70)
sum(titanic1$Age %in% 70:80)/sum(titanic$Age %in% 70:80)
titanic %>% filter(fare %in% 0)
head(titanic)
titanic %>% filter(fare %in% 0.0)
titanic %>% filter(Fare %in% 0)
titanic %>% filter(!Fare %in% 0)
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot() + geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot() + geom_jitter(Fare,Survived)
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,group=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot()
color
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_y_continuous(trans="log2") + geom_boxplot()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot() +geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot(y=..count..) +geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,y=..count..,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot() +geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot()
+ scale_x_continuous(trans="log2") + geom_boxplot()
+ scale_x_continuous(trans="log2") + geom_boxplot() + geom_jitter(aes(x=Fare,y=..count..))
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2))
+ scale_x_continuous(trans="log2") + geom_boxplot() + geom_jitter(aes(x=Fare,y=..count..))
+ scale_x_continuous(trans="log2")  + geom_jitter(aes(x=Fare,y=..count..))
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2))
+ scale_x_continuous(trans="log2")  + geom_jitter(aes(x=Fare,y=..count..))
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2")  + geom_jitter(aes(x=Fare,y=..count..))
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2")  + geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,..count..,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2")  + geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,Sex,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2")  + geom_jitter()
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot() +
  scale_y_continuous(trans = "log2") +
  geom_jitter(alpha = 0.2)
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,Surviv,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2")  + geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,Surviv,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2")  + geom_jitter()
titanic %>% filter(!Fare %in% 0) %>% ggplot(aes(Fare,color=Survived,alpha=0.2)) + scale_x_continuous(trans="log2") + geom_boxplot()
titanic %>% filter(!is.na(Age)) %>% ggpot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha=0.2) + facet_grid(Sex~Pclass)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha=0.2) + facet_grid(Sex~Pclass)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density() + facet_grid(Sex~Pclass)
titanic %>% filter(!is.na(Age)) %>% ggplot(aes(Age,y=..count..,fill=Survived)) + geom_density(alpha=0.2) + facet_grid(Sex~Pclass)
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar()
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived,fill=Pclass)) +geom_bar(position = position_fill())
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar(position = position_fill())
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar()
titanic %>% ggplot(aes(Pclass,fill=Survived)) +geom_bar(position = position_fill())
titanic %>% ggplot(aes(Survived,fill=Pclass)) +geom_bar(position = position_fill())
sam<-rep(c("cyan","magenda","yellow"),timmes=c(3,5,7))
sam
sam<-rep(c("cyan","magenda","yellow"),times=c(3,5,7))
sam
mean(sam=="cyan")
mean(sam!="cyan")
prob.sam
prob.table(sam)
mean(sam=="cyan")
x<-0.2*(12/14)
x
x<-0.2*0.8
x
