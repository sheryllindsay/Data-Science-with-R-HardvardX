#section 2

#2.3

#assesment

#5
install.packages("do")
install.packages("broom")
library("do")
library("broom")
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = dat)
  sum_fit <- summary(fit)
  data.frame(slope = sum_fit$coefficients[2, "Estimate"], 
             se = sum_fit$coefficients[2, "Std. Error"],
             pvalue = sum_fit$coefficients[2, "Pr(>|t|)"])
}

dat %>% group_by(HR) %>% do(get_slope(.))

#7
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 
dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 

#assesment part 2
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

#8
#a
x<-galton %>% group_by(pair)
galton %>% filter(pair=="father_daughter") %>% nrow()
#b
galton %>% filter(pair=="mother_son") %>% nrow()


#9
x1<-x %>% filter(pair=="father_daughter") 
cor(x1$childHeight,x1$parentHeight)

x2<-x %>% filter(pair=="mother_daughter") 
cor(x2$childHeight,x2$parentHeight)

x3<-x %>% filter(pair=="father_son") 
cor(x3$childHeight,x3$parentHeight)

x4<-x %>% filter(pair=="mother_son") 
cor(x4$childHeight,x4$parentHeight)

#10

#part a
summary(x1 %>% lm(childHeight ~ parentHeight, data=. ))

x4 %>% lm(childHeight ~ parentHeight, data=. )

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight", pair == "mother_son") %>%
  pull(estimate)

#part b
summary(x1 %>% lm(childHeight ~ parentHeight, data=. )) %>% .$coefficients
summary(x2 %>% lm(childHeight ~ parentHeight, data=. )) %>% .$coefficients
summary(x3 %>% lm(childHeight ~ parentHeight, data=. )) %>% .$coefficients
summary(x4 %>% lm(childHeight ~ parentHeight, data=. )) %>% .$coefficients

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
  filter(term == "parentHeight", pair == "mother_son")
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
  filter(term == "parentHeight", pair == "mother_daughter")
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
  filter(term == "parentHeight", pair == "father_son")
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE))%>%
  filter(term == "parentHeight", pair == "father_daughter")



galton %>%
  group_by(pair) %>%
  do(tidy(lm(parentHeight ~ childHeight, data = .), conf.int = TRUE))%>%
  filter(term == "childHeight", pair == "mother_son")
galton %>%
  group_by(pair) %>%
  do(tidy(lm(parentHeight ~ childHeight, data = .), conf.int = TRUE))%>%
  filter(term == "childHeight", pair == "mother_daughter")
galton %>%
  group_by(pair) %>%
  do(tidy(lm(parentHeight ~ childHeight, data = .), conf.int = TRUE))%>%
  filter(term == "childHeight", pair == "father_daughter")
galton %>%
  group_by(pair) %>%
  do(tidy(lm(parentHeight ~ childHeight, data = .), conf.int = TRUE))%>%
  filter(term == "childHeight", pair == "father_son")


galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05)
#------------------------------------------------------------------------------

#2.4
library("broom")
# linear regression with two variables
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
  lm(R ~ BB + HR, data = .)
tidy(fit, conf.int = TRUE)

# regression with BB, singles, doubles, triples, HR
fit <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(BB = BB / G, 
         singles = (H - X2B - X3B - HR) / G, 
         doubles = X2B / G, 
         triples = X3B / G, 
         HR = HR / G,
         R = R / G) %>%  
  lm(R ~ BB + singles + doubles + triples + HR, data = .)
coefs <- tidy(fit, conf.int = TRUE)
coefs

# predict number of runs for each team in 2002 and plot
Teams %>% 
  filter(yearID %in% 2002) %>% 
  mutate(BB = BB/G, 
         singles = (H-X2B-X3B-HR)/G, 
         doubles = X2B/G, 
         triples =X3B/G, 
         HR=HR/G,
         R=R/G)  %>% 
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()

# average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>% 
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% 
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players <- Batting %>% filter(yearID %in% 1999:2001) %>% 
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G, 
            triples = sum(X3B)/G, 
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

# plot player-specific predicted runs
qplot(R_hat, data = players, geom = "histogram", binwidth = 0.5, color = I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))

# add players' first and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% 
  top_n(10) 

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

# remake plot without players that debuted after 1998
library(lubridate)
players %>% filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) + 
  geom_point() +
  scale_x_log10()

#linear programming

library(reshape2)
install.packages("lpSolve")
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

#assignments

#3
#BB/PA  +  (Singles+2Doubles+3Triples+4HR)/AB
?Teams
TeamA<-2 +(4+2+4)
TeamB<-1 +(6+4+3)

#9
#part A
head(Teams)
library("broom")
library("do")
x<-Teams%>% filter(yearID==1971) %>% mutate(BB = BB/G, HR = HR/G,  R = R/G) %>% lm(R ~ HR+BB , data=.) 
tidy(x, conf.int = TRUE) %>% filter(p.value>0.05)

#10
x<-Teams%>% filter(yearID %in% 1961:2018) %>% group_by(yearID) %>% do(tidy(lm(R ~ HR+BB , data=.),conf.int=TRUE)) %>% filter(term=="BB") %>% ungroup()
head(x)
x %>% ggplot(aes(yearID,estimate)) + geom_point() + geom_smooth(method="lm")

#11
summary(x %>% lm(estimate ~ yearID, data=.))

x %>%lm(estimate ~ yearID, data = .) %>% tidy()
#----------------------------------------------------------------------------------------------------------