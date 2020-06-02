#1
#In 1999 in England Sally Clark was found guilty of the murder 
#of two of her sons. Both infants were found dead in the morning,
#one in 1996 and another in 1998, and she claimed the cause of 
#death was sudden infant death syndrome (SIDS). No evidence of 
#physical harm was found on the two infants so the main piece 
#of evidence against her was the testimony of Professor Sir Roy 
#Meadow, who testified that the chances of two infants dying of 
#SIDS was 1 in 73 million. He arrived at this figure by finding 
#that the rate of SIDS was 1 in 8,500 and then calculating that
#the chance of two SIDS cases was 8,500 × 8,500 ??? 73 million.
#Based on what we've learned throughout this course, which statement
#best describes a potential flaw in Sir Meadow's reasoning?

# ANS : Sir Meadow assumed the second death was independent of the 
#       first son being affected, thereby ignoring possible genetic causes.

#2
#Let's assume that there is in fact a genetic component to SIDS and the 
#the probability of Pr(second case of SIDS???first case of SIDS)=1/100, is much higher than 1 in 8,500.
#What is the probability of both of Sally Clark's sons dying of SIDS?

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Calculate the probability of both sons dying of SIDS. Print this value to the console.
Pr_2*Pr_1

#4
#Assume that the probability of a murderer finding a way to kill her two 
#children without leaving evidence of physical harm is:
#Pr(two children found dead with no evidence of harm???mother is a murderer)=0.50
#Assume that the murder rate among mothers is 1 in 1,000,000.
#Pr(mother is a murderer)=1/1,000,000
#According to Bayes' rule, what is the probability of:
#Pr(mother is a murderer???two children found dead with no evidence of harm)

# Define `Pr_1` as the probability of the first son dying of SIDS
Pr_1 <- 1/8500

# Define `Pr_2` as the probability of the second son dying of SIDS
Pr_2 <- 1/100

# Define `Pr_B` as the probability of both sons dying of SIDS
Pr_B <- Pr_1*Pr_2

# Define Pr_A as the rate of mothers that are murderers
Pr_A <- 1/1000000

# Define Pr_BA as the probability that two children die without evidence of harm, given that their mother is a murderer
Pr_BA <- 0.50

# Define Pr_AB as the probability that a mother is a murderer, given that her two children died with no evidence of physical harm. Print this value to the console.
Pr_AB<- Pr_BA*Pr_A/Pr_B
Pr_AB

#5
#After Sally Clark was found guilty, the Royal Statistical 
#Society issued a statement saying that there was "no statistical basis"
#for the expert's claim. They expressed concern at the "misuse of statistics
#in the courts". Eventually, Sally Clark was acquitted in June 2003.
#In addition to misusing the multiplicative rule as we saw earlier, what else did Sir Meadow miss?

# ANS : He did not take into account how rare it is for a mother to murder her children.

#6
#Florida is one of the most closely watched states in the U.S. election because it has
#many electoral votes and the election is generally close. Create a table with the poll
#spread results from Florida taken during the last days before the election using the sample code.
#The CLT tells us that the average of these spreads is approximately normal.
#Calculate a spread average and provide an estimate of the standard error.

# Load the libraries and poll data
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

# Create an object `polls` that contains the spread of predictions for each candidate in Florida during the last polling days
polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Examine the `polls` object using the `head` function
head(polls)

# Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`). Print the results to the console.
results<-polls %>% summarize(avg=mean(spread),se=sd(spread)/sqrt(length(spread)))
results

#7
#Assume a Bayesian model sets the prior distribution for Florida's 
#election night spread d to be normal with expected value ?? and standard deviation ??.
#What are the interpretations of ?? and ???

# ANS: ?? and ?? summarize what we would predict for Florida before seeing any polls.

#8
#The CLT tells us that our estimate of the spread d^ has a normal
#distribution with expected value d and standard deviation ??, which
#we calculated in a previous exercise.
#Use the formulas for the posterior distribution to calculate the 
#expected value of the posterior distribution if we set ??=0 and ??=0.01.

# The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

# Define `mu` and `tau`
mu <- 0
tau <- 0.01

# Define a variable called `sigma` that contains the standard error in the object `results`
sigma<-results$se[1]

# Define a variable called `Y` that contains the average in the object `results`
Y<-results$avg[1]

# Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B<-(sigma^2)/(sigma^2+tau^2)
B

# Calculate the expected value of the posterior distribution
B*mu+(1-B)*Y

#9
#Compute the standard error of the posterior distribution.

# Here are the variables we have defined
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

# Compute the standard error of the posterior distribution. Print this value to the console.
se<-sqrt(1/((1/(sigma^2))+(1/(tau^2))))
se

#10
#Using the fact that the posterior distribution is normal, create an interval
#that has a 95% of occurring centered at the posterior expected value. Note 
#that we call these credible intervals.

# Here are the variables we have defined in previous exercises
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-(B*mu+(1-B)*Y)+c(-qnorm(0.975),qnorm(0.975))*se
ci

#11
#According to this analysis, what was the probability that Trump wins Florida?
# Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

# Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0,exp_value,se)

#12
#We had set the prior variance ?? to 0.01, reflecting that these races are often close.
#Change the prior variance to include values ranging from 0.005 to 0.05
#and observe how the probability of Trump winning Florida changes by making a plot.

# Define the variables from previous exercises
mu <- 0
sigma <- results$se
Y <- results$avg

# Define a variable `taus` as different values of tau
taus <- seq(0.005, 0.05, len = 100)

# Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc<-function(taus){
  B<-(sigma^2/(sigma^2+taus^2))
  pnorm(0,B*mu+(1-B)*Y,sqrt(1/((1/(sigma^2))+(1/(taus^2)))))
}

# Create a vector called `ps` by applying the function `p_calc` across values in `taus`
ps<-sapply(taus,p_calc)

# Plot `taus` on the x-axis and `ps` on the y-axis
plot(taus,ps)

#13



