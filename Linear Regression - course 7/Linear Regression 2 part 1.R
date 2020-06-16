# Section 2: Linear models
#2.1
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope


# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 

#-------------------------------------------------------------------------------

#2.2
#lse and random variation
# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
install.packages("gridExtra")
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>% 
  lm(son ~ father, data = .) %>% 
  summary %>%
  .$coef

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

#advanced topics

lse %>% summarize(cor(beta_0, beta_1))
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 

#assesment

#1
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
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)

#3
?Teams
Team<-Teams %>% filter(yearID %in% 1961:2001) 
Team<-Team %>% mutate(R_G=R/G,HR_G=HR/G,BB_G=BB/G) 

#Team <- Team%>% mutate(Y_hat = predict(lm(HR_G ~ BB_G, data=.))) 
#Team %>% lm(R_G ~ Y_hat, data = .) %>% .$coef 

lm(R_G ~ HR_G+BB_G, data = Team) %>% .$coef


#6
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# necessary for 7-8
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

#7
x<-female_heights %>% lm(mother ~ daughter,data=.) %>% .%coef

#8
model <- lm(mother ~ daughter, data = female_heights)
predictions <- predict(model)
female_heights<-female_heights %>% mutate(y=predictions)
female_heights[1,]

#9-12 pre code
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)


#9
head(Batting)

means <- Batting %>%  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>% group_by(playerID)  %>% mutate(mean_singles=mean(singles),mean_bb=mean(bb))  

mean_final<- means%>% summarize(mean_singles=mean(singles),mean_bb=mean(bb)) 

nrow(mean_final %>% filter(mean_singles>0.2)) 
nrow(mean_final %>% filter(mean_bb>0.2)) 

#10
meansx <- Batting %>%  filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100)  %>% group_by(playerID)  %>% summarize(mean_singles=mean(singles),mean_bb=mean(bb)) %>% select(playerID,mean_singles,mean_bb)

bat<-inner_join(meansx,bat_02,by="playerID")
head(bat)
cor(bat$singles,bat$mean_singles)
cor(bat$bb,bat$mean_bb)
nrow(bat)
#unique(bat$yearID)

#11
bat %>% ggplot(aes(singles,mean_singles)) +geom_point()
bat %>% ggplot(aes(bb,mean_bb)) +geom_point()

#12
bat %>% lm(singles ~ mean_singles, data=.) %>% .$coef
bat %>% lm(bb ~ mean_bb, data=.) %>% .$coef

#---------------------------------------------------------------------------
