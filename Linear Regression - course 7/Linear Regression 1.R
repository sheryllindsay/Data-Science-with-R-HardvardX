#Section 1: Introduction to Regression

#1.1

library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) # good relationship

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) # relationship is bad

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5) #okayish

#assesment 1.1

#4
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

#5
?Teams

#6
x<-Teams %>% filter(yearID %in% 1961:2001) 
x%>% mutate(R_per_game= R/G, AB_per_game= AB/G) %>% ggplot(aes(R_per_game, AB_per_game))+geom_point()

#7
x %>% mutate(win_rate=W/G,error_rate=E/G) %>% ggplot(aes(win_rate,error_rate))+geom_point()

#8
x %>% mutate(x3b_rate=X3B/G,x2b_rate=X2B/G) %>% ggplot(aes(x3b_rate,x2b_rate))+geom_point()
#--------------------------------------------------------------------------------------------

#1.2 correlation

library(tidyverse)
install.packages("HistData")
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)
data(galton_heights)

R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

#assesment

#7
?Teams
library(Lahman)
x<-Teams %>% filter(yearID %in% 1961:2001) %>% mutate(runs=R/G,bats=AB/G) %>% summarize(r=cor(runs,bats)) #%>% pull(r)
x

#8
x<-Teams %>% filter(yearID %in% 1961:2001) %>% mutate(wins=W/G,errors=E/G) %>% summarize(r=cor(wins,errors)) #%>% pull(r)
x

#9
x<-Teams %>% filter(yearID %in% 1961:2001) %>% mutate(x3b_rate=X3B/G,x2b_rate=X2B/G) %>% summarize(r=cor(x3b_rate,x2b_rate)) #%>% pull(r)
x
#---------------------------------------------------------------------------------------------

#1.3 Stratification and variance explained

#assesment

#7
slope<-0.5*(3/2)
slope

set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#8
Mean_m<-mean(female_heights$mother)
Mean_d<-mean(female_heights$daughter)
sd_m<-sd(female_heights$mother)
sd_d<-sd(female_heights$daughter)
corr<-cor(female_heights$mother,female_heights$daughter)

#9
slope<-corr*(sd_d/sd_m)
intercept<-Mean_d-(slope*Mean_m)

#10
corr^2 * 100

#11
#y=mx+c
y<-slope*60+intercept
y
#--------------------------------------------------------------------------
