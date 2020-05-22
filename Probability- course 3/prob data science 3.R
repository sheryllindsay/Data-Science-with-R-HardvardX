#NOTE : for this exercise use the following formulae
#Expected value of a random variable: 
 # ap+b(1???p)
#Expected value of the sum of n draws of a random variable: 
 # n*(ap+b(1???p))
#Standard deviation of an urn with two values: 
 # ???b-a??? * sqrt(p(1???p))
#Standard error of the sum of n draws of a random variable:
 # sqrt(n) * ???b-a??? * sqrt(p(1???p))

#In American roulette, the payout for winning on green is $17. This means that
#if you bet $1 and it lands on green, you get $17 as a prize.

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)

# The variables 'green', 'black', and 'red' contain the number of pockets for each color
green <- 2
black <- 18
red <- 18

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- green / (green+black+red)

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- (black+red) / (green+black+red)

# Create a model to predict the random variable `X`, your winnings from betting on green. Sample one time.
X<-sample(c(17,-1),1,replace=TRUE,prob=c(p_green,p_not_green))

# Calculate the EXPECTED OUTCOME if you win $17 if the ball lands on
#green and you lose $1 if the ball doesn't land on green
X<-sum(sample(c(17,-1),1,replace=TRUE,prob=c(p_green,p_not_green)))*p_green

# Compute the standard error of the random variable
SE<-abs(-1-17)*(sqrt(p_green*p_not_green))

#create a random variable S that sums your winnings after betting on green 1,000 times.

# Define the number of bets using the variable 'n'
n<-1000
# Create a vector called 'X' that contains the outcomes of 1000 samples
X<-sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))
# Assign the sum of all 1000 outcomes to the variable 'S'
S<-sum(X)

# Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on
#green and you lose $1 when the ball doesn't land on green
X<-sample(c(17,-1),1,replace=TRUE,prob=c(p_green,p_not_green))
X<-n*sum(X)*p_green

# Compute the standard error of the sum of 1,000 outcomes
sqrt(n)*abs(-1-17)*sqrt(p_green*p_not_green)

#---------------------------------------------------------------------------------------------------------

#1#What is the probability that you end up winning money if you bet on green 100 times?

# Assign a variable `p_green` as the probability of the ball landing in a green pocket
p_green <- 2 / 38

# Assign a variable `p_not_green` as the probability of the ball not landing in a green pocket
p_not_green <- 1-p_green

# Define the number of bets using the variable 'n'
n <- 100

# Calculate 'avg', the expected outcome of 100 spins if you win $17 when the ball 
#lands on green and you lose $1 when the ball doesn't land on green
avg <- n * (17*p_green + -1*p_not_green)

# Compute 'se', the standard error of the sum of 100 outcomes
se <- sqrt(n) * (17 - -1)*sqrt(p_green*p_not_green)

# Using the expected value 'avg' and standard error 'se', compute the probability 
#that you win money betting on green 100 times.
1-pnorm(0,avg,se)

#2#Create a Monte Carlo simulation that generates 10,000 outcomes of S, the sum of 100 bets.
#Compute the average and standard deviation of the resulting list and compare them to the 
#expected value (-5.263158) and standard error (40.19344) for S that you calculated previously.

# Define the number of bets using the variable 'n'
n <- 100
# The variable `B` specifies the number of times we want the simulation to run. Let's run the Monte Carlo simulation 10,000 times.
B <- 10000
set.seed(1)
# Create an object called `S` that replicates the sample code for `B` iterations and sums the outcomes.
S<-replicate(B,sum(sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))))
# Compute the average value for 'S'
mean(S)
# Calculate the standard deviation of 'S'
sd(S)
# Calculate the proportion of outcomes in the vector `S` that exceed $0
mean(S>0)

#3#Now create a random variable Y that contains your average winnings per bet after betting on green 10,000 times.

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling.
set.seed(1)
# Define the number of bets using the variable 'n'
n <- 10000
# Create a vector called `X` that contains the outcomes of `n` bets
X<-sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))
# Define a variable `Y` that contains the mean outcome per bet. Print this mean to the console.
Y<-mean(X)
Y

#4# Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
avg<-17*p_green- 1*p_not_green

#5# Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
se<-abs(-17-1) * sqrt(p_green*p_not_green) / sqrt(n)
# Given this average and standard error, determine the probability of winning more than $0. Print the result to the console.
1-pnorm(0,avg,se)

#6#Create a Monte Carlo simulation that generates 10,000 outcomes of S,
#the average outcome from 10,000 bets on green.

#Compute the average and standard deviation of the resulting list to confirm the 
#results from previous exercises using the Central Limit Theorem.

# The variable `n` specifies the number of independent bets on green
n <- 10000
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000
# Use the `set.seed` function to make sure your answer matches the expected result after random number generation
set.seed(1)
# Generate a vector `S` that contains the the average outcomes of 10,000 bets modeled 10,000 times
S<-replicate(B,mean(sample(c(17,-1),n,replace=TRUE,prob=c(p_green,p_not_green))))
# Compute the average of `S`
mean(S)
# Compute the standard deviation of `S`
sd(S)
# Compute the proportion of outcomes in the vector 'S' where you won more than $0
mean(S>0)

#----------------------------------------------------------------


#An old version of the SAT college entrance exam had a -0.25 point penalty for 
#every incorrect answer and awarded 1 point for a correct answer. The quantitative
#test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses
#answers by guessing for all questions on the test.

#1a#What is the probability of guessing correctly for one question?
prob_r<-1/5
prob_nr<-1-prob_r

#1b#What is the expected value of points for guessing on one question?
1*prob_r-0.25*prob_nr

#1c#What is the expected score of guessing on all 44 questions?
avg<-44*(1*prob_r-0.25*prob_nr)

#1d#What is the standard error of guessing on all 44 questions?
se<-(sqrt(prob_nr*prob_r)*abs(-.25-1)) *sqrt(44)

#1e#Use the Central Limit Theorem to determine the probability 
#that a guessing student scores 8 points or higher on the test.
1-pnorm(8,avg,se)

#1e#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
#What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")
S<-replicate(10000,sum(sample(c(1,-0.25),44,replace = TRUE,prob=c(prob_r,prob_nr))))
mean(S>8)

#---------------------------------------------

#The SAT was recently changed to reduce the number of multiple choice options
#from 5 to 4 and also to eliminate the penalty for guessing.


prob_r<-1/4
prob_nr<-1-prob_r

#2a#What is the expected value of the score when guessing on this new test?
avg<-44*(1*prob_r+0*prob_nr)

se<-sqrt(44)*(sqrt(prob_nr*prob_r)*abs(0-1)) 
p <- seq(0.25, 0.95, 0.05)
qnorm(0.99,avg,se)
qnorm(0.95,avg,se)
qnorm(0.05,avg,se)
1-pnorm(35,avg,se)

#quite complicated. asked me to take the p values as probabilities of getting answers 
#rifgt or wrong in the test. have to calculate se and avg using them and trying to see which p value
# gives what probability for getting a score of 35 or above. and which of these probability 
#is greater than 0.80 or 80%

p <- seq(0.25, 0.95, 0.05)
set.seed(1, sample.kind = "Rounding")
avg_se<-function(n){
  right<-n
  not_right<-1-right
  avg<-44*(1*right)
  se<-sqrt(44)*(sqrt(right*not_right)*abs(0-1))
  1-pnorm(35,avg,se)
}
ans<-sapply(p,avg_se)
min(which(ans>0.80))
p[13]

#------------------------------------------------------------------------------------

#A casino offers a House Special bet on roulette,
#which is a bet on five pockets (00, 0, 1, 2, 3) out
#of 38 total pockets. The bet pays out 6 to 1. In other 
#words, a losing bet yields -$1 and a successful bet yields
#$6. A gambler wants to know the chance of losing money if he 
#places 500 bets on the roulette House Special.

right<-5/38
not_right<-1-right

#3a#What is the expected value of the payout for one bet?
(6*right-1*not_right)

#3b#What is the standard error of the payout for one bet?
(sqrt(right*not_right)*abs(-1-6))

#3c#What is the expected value of the average payout over 500 bets?

#twist question cus average payout of mutiple bets is the same as expected value of one bet
(6*right-1*not_right)

#3d#What is the standard error of the average payout over 500 bets?
(sqrt(right*not_right)*abs(-1-6))/sqrt(500)

#3e#What is the expected value of the sum of 500 bets?
avg<-(6*right-1*not_right)*500

#3f#What is the standard error of the sum of 500 bets?
se<-(sqrt(right*not_right)*abs(-1-6))*sqrt(500)

#3g#Use pnorm() with the expected value of the sum and standard error of 
#the sum to calculate the probability of losing money over 500 bets,  Pr(X???0) .
pnorm(0,avg,se)













