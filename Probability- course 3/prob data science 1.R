sam<-rep(c("cyan","magenda","yellow"),times=c(3,5,7))
sam
mean(sam=="cyan")
mean(sam!="cyan")
prob.table(sam)
x<-0.2*0.8
x
install.packages("tidyverse")
install.packages("gtools")
library(gtools)
library(tidyverse)
library(dplyr)
install.packages("dplyr")

#For each iteration of the Monte Carlo simulation, within a replicate() loop, select 3 
#runners representing the 3 medalists and check whether they are all from
#Jamaica. Repeat this simulation 10,000 times. Set the seed to 1 before running the loop.
set.seed(1)
results<-replicate(10000,{
  runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
  simulated<- sample(runners,3)
  sum(simulated=="Jamaica")==3
})
mean(results)

#---------------------------------------------------
#Two teams, say the Cavs and the Warriors, are playing a seven game
#championship series. The first to win four games wins the series. The 
#teams are equally good, so they each have a 50-50 chance of winning each game.
#If the Cavs lose the first game, what is the probability that they win the series?
# Assign a variable 'n' as the number of remaining games.
n<-6

# Assign a variable `outcomes` as a vector of possible game outcomes, where 0 indicates a loss 
#and 1 indicates a win for the Cavs.
outcomes<-sample(c("lose"=0,"win"=1), prob = c(0.5, 0.5))
outcomes

# Assign a variable `l` to a list of all possible outcomes in all remaining games. Use the
#`rep` function on `list(outcomes)` to create list of length `n`.
l<-rep(list(outcomes),times=n)
l

# Create a data frame named 'possibilities' that contains all combinations of possible 
#outcomes for the remaining games.
possibilities<-expand.grid(l)
possibilities
# Create a vector named 'results' that indicates whether each row in the data frame 
#'possibilities' contains enough wins for the Cavs to win the series.
results<-rowSums(possibilities)>3
# Calculate the proportion of 'results' in which the Cavs win the series. Print the outcome to the console.
mean(results)

#--------------------------------------------------------
  #Two teams, A and B, are playing a seven series game series. Team A is better 
  #than team B and has a p>0.5 chance of winning each game.
  # Let's assign the variable 'p' as the vector of probabilities that team A will win.
  p <- seq(0.5, 0.95, 0.025)

# Given a value 'p', the probability of winning the series for the underdog team B can be computed with the following function based on a Monte Carlo simulation:
prob_win <- function(p){
  B <- 10000
  result <- replicate(B, {
    b_win <- sample(c(1,0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_win)>=4
  })
  mean(result)
}
Pr<-sapply(p,prob_win)
plot(p,Pr)

# Apply the 'prob_win' function across the vector of probabilities that team A will win to determine 
#the probability that team B will win. Call this object 'Pr'.


# Plot the probability 'p' on the x-axis and 'Pr' on the y-axis.
#--------------------------------------------------------------------
  # This line of example code simulates four independent random games where the Celtics
#either lose or win. Copy this example code to use within the `replicate` function.
  
  # The variable 'B' specifies the number of times we want the simulation to run.
#Let's run the Monte Carlo simulation 10,000 times.
  B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
# Create an object called `celtic_wins` that replicates two steps for B iterations: (1)
#generating a random four-game series `simulated_games` using the example code, then 
#(2) determining whether the simulated series contains at least one win for the Celtics.
set.seed(1)
celtic_wins<-replicate(B,any(simulated_games=="win"))
all(celtic_wins)

# Calculate the frequency out of B iterations that the Celtics won at least one game.
#Print your answer to the console.


set.seed(1)
celtic_wins <- replicate(10000, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games == "win")
})
all(celtic_wins)
mean(celtic_wins)
#----------------------------------------------------------------------------

#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink.
#He currently offers a choice of 1 entree from a list of 
#6 options, a choice of 2 different sides from a list of 6 options,
#and a choice of 1 drink from a list of 2 options.

#How many meal combinations are possible with the current menu?
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))

#How many combinations are possible if he expands his original special to 3 drink options?
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(3,1))

#How many meal combinations are there if customers can choose from 6 entrees,
#3 drinks, and select 3 sides from the current 6 options?
nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))

#- Write a function that takes a number of entree choices and returns the number 
#of meal combinations possible given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
#- Use sapply() to apply the function to entree option counts ranging from 1 to 12.

entree <- function(n){
  nrow(combinations(n,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
}
x<-sapply(1:12,entree)
x

#choosing 2:12 side options instead of entree 
entree <- function(n){
  nrow(combinations(6,1))*nrow(combinations(n,2))*nrow(combinations(3,1))
}
x<-sapply(2:12,entree)
x

#more ethical code
library(tidyverse)

entree_choices <- function(x){
  x * nrow(combinations(6,2)) * 3
}

combos <- sapply(1:12, entree_choices)

data.frame(entrees = 1:12, combos = combos) %>%
  filter(combos > 365) %>%
  min(.$entrees)
#------------------------------------------------------------------
#Each row contains one group of the experiment. Each group has a different
#combination of age, alcohol consumption, and tobacco consumption. The number of cancer cases and number
#of controls (individuals without cancer) are reported for each group.
head(esoph)
is.factor(esoph$agegp)
nlevels(esoph$agegp)
nlevels(esoph$tobgp)
nlevels(esoph$alcgp)
6*4*4
summary(esoph)

#How many groups are in the study?
nrow(esoph)

#How many cases are there?
all_cases<-sum(esoph$ncases)
all_cases
#How many controls are there?
all_control<-sum(esoph$ncontrols)

#What is the probability that a subject in the highest alcohol consumption group is a cancer case?
levels(esoph$alcgp)
ind<-which(esoph$alcgp=="0-39g/day")
ind  
sum(esoph$ncases[ind])
sum(esoph$ncontrols[ind])
sum(esoph$ncases[ind])+sum(esoph$ncontrols[ind])
29/444
esoph$tobgp[ind]

#Given that a person is a case, what is the probability that they smoke 10g or more a day?

levels(esoph$tobgp)
ind<-which(esoph$tobgp!="0-9g/day")
ind  
sum(esoph$ncases[ind])/sum(esoph$ncases)

#For cases, what is the probability of being in the highest alcohol group?
levels(esoph$alcgp)
ind<-which(esoph$alcgp=="120+")
ind  
sum(esoph$ncases[ind])/sum(esoph$ncases)

##For cases, what is the probability of being in the highest alcohol or tobaco group? 
levels(esoph$alcgp)
levels(esoph$tobgp)
ind<-which(esoph$alcgp=="120+" | esoph$tobgp=="30+")
ind  
x<-sum(esoph$ncases[ind])/sum(esoph$ncases)
x
y<-sum(esoph$ncontrols[ind])/sum(esoph$ncontrols)
y
#How many times more likely are cases than controls to be in the highest alcohol group or the highest tobacco group?
x/y

#For controls, what is the probability of being in the highest alcohol group?
levels(esoph$alcgp)
ind<-which(esoph$alcgp=="120+")
ind  
sum(esoph$ncontrols[ind])/sum(esoph$ncontrols)
sum(esoph$ncases[ind])/sum(esoph$ncases)

#How many times more likely are cases than controls to be in the highest alcohol group?
x<-sum(esoph$ncontrols[ind])/sum(esoph$ncontrols)
y<-sum(esoph$ncases[ind])/sum(esoph$ncases)
x/y
0.225/0.06871795
