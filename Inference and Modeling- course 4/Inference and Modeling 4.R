#1
#We have been using urn models to motivate the use
#of probability models. However, most data science 
#applications are not related to data obtained from 
#urns. More common are data that come from individuals.
#Probability plays a role because the data come from a 
#random sample. The random sample is taken from a population
#and the urn serves as an analogy for the population.
#Let's revisit the heights dataset. For now, consider
#x to be the heights of all males in the data set. 
#Mathematically speaking, x is our population. Using the 
#urn analogy, we have an urn with the values of x in it.

#What are the population average and standard deviation of our population?

# Load the 'dslabs' package and data contained in 'heights'
library(dslabs)
data(heights)

# Make a vector of heights from all males in the population
x <- heights %>% filter(sex == "Male") %>%.$height

# Calculate the population average. Print this value to the console.
mean(x)

# Calculate the population standard deviation. Print this value to the console.
sd(x)

#2
#Call the population average computed above ?? and the standard deviation ??.
#Now take a sample of size 50, with replacement, and construct an estimate for ?? and ??.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X<-sample(x,N,replace=TRUE)

# Calculate the sample average. Print this value to the console.
mean(X)

# Calculate the sample standard deviation. Print this value to the console.
sd(X)

#3
#What does the central limit theory tell us about the sample
#average and how it is related to ??, the population average?

# ANS: It is a random variable with expected value ?? and standard error ??/???(N)

#4
#We will use X¯ as our estimate of the heights in the
#population from our sample size N. We know from previous
#exercises that the standard estimate of our error X¯????? is ??/N?????????.
#Construct a 95% confidence interval for ??.

# The vector of all male heights in our population `x` has already been loaded for you. You can examine the first six elements using `head`.
head(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `X` as a random sample from our population `x`
X <- sample(x, N, replace = TRUE)

# Define `se` as the standard error of the estimate. Print this value to the console
se<-sd(X)/sqrt(N)
se

# Construct a 95% confidence interval for the population average based 
#on our sample. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-mean(X) + c(-qnorm(0.975),qnorm(0.975))*se
ci

#5
#Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals 
#as you have just done. What proportion of these intervals include ???

# Define `mu` as the population average
mu <- mean(x)

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Define `N` as the number of people measured
N <- 50

# Define `B` as the number of times to run the model
B <- 10000

# Define an object `res` that contains a logical vector for simulated intervals that contain mu
res<-replicate(B,{
  X<-sample(x,N,replace=TRUE)
  interval<-mean(X)+c(-qnorm(0.975),qnorm(0.975))*(sd(X)/sqrt(N))
  between(mu,interval[1],interval[2])
})

# Calculate the proportion of results in `res` that include mu. Print this value to the console.
mean(res)

#6
#Lets consider two pollsters that conducted daily polls
#and look at national polls for the month before the election.
#Is there a poll bias? Make a plot of the spreads for each poll.

# Load the libraries and data you need for the following exercises
library(dslabs)
library(dplyr)
library(ggplot2)
data("polls_us_election_2016")

# These lines of code filter for the polls we want and calculate the spreads
polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

# Make a boxplot with points of the spread for each pollster
polls %>% ggplot(aes(pollster,spread)) + geom_boxplot()+geom_point()

#7
#The data do seem to suggest there is a difference between 
#the pollsters. However, these data are subject to variability.
#Perhaps the differences we observe are due to chance. Under the 
#urn model, both pollsters should have the same expected value: the election day difference, d.
#We will model the observed data Yij in the following way:
# Yij=d+bi+??ij
#with i=1,2 indexing the two pollsters, bi the bias 
#for pollster i, and ??ij poll to poll chance variability. 
#We assume the ?? are independent from each other, have expected
#value 0 and standard deviation ??i regardless of j.

#Which of the following statements best reflects what we need to know to determine if our data fit the urn model?

#ANS:
#Is b1???b2?


#8
#We modelled the observed data Yij as:
#Yij=d+bi+??ij
#On the right side of this model, only ??ij is a random variable. The other two values are constants.
#What is the expected value of Y1j?

#ANS : d+b1

#9
#Suppose we define Y¯1 as the average of poll results from the 
#first poll and ??1 as the standard deviation of the first poll.
#What is the expected value and standard error of Y¯1?

# ANS: The expected value is d+b1 and the standard error is ??1/???N1

#10
#Now we define Y¯2 as the average of poll results from the second poll.
#What is the expected value and standard error of Y¯2?

# ANS : The expected value is d+b2 and the standard error is ??2/???N2

#11
#Using what we learned by answering the previous questions, what is the expected value of Y¯2???Y¯1?

#ANS : b2???b1

#12
#Using what we learned by answering the questions above, what is the standard error of Y¯2???Y¯1?

#ANS : ???(??22/N2+??21/N1)

#13
#The answer to the previous question depends on ??1 and ??2,
#which we don't know. We learned that we can estimate these 
#values using the sample standard deviation.
#Compute the estimates of ??1 and ??2.

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `sigma` that contains a column for `pollster` and a column for `s`, the standard deviation of the spread
sigma<-polls %>% group_by(pollster) %>% summarise(s=sd(spread))


# Print the contents of sigma to the console
sigma

#14
#What does the central limit theorem tell us about the distribution of the differences 
#between the pollster averages, Y¯2???Y¯1?

# ANS : If we assume N2 and N1 are large enough, Y¯2 and Y¯1, and their difference, are approximately normal.

#15
#We have everything we need to answer our initial question: is b2???b1 different from 0?
#Construct a 95% confidence interval for the difference b2 and b1. Does this interval contain zero?

# The `polls` data have already been loaded for you. Use the `head` function to examine them.
head(polls)

# Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res<-polls %>% group_by(pollster) %>% summarize(average=mean(spread), sd= sd(spread),nu=length(spread))
res

# Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate<-res$average[2]-res$average[1]

# Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.

se_hat<-sqrt((res$sd[1]^2/res$nu[1])+(res$sd[2]^2/res$nu[2]))
se_hat

# Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-estimate + c(-qnorm(0.975),qnorm(0.975))*se_hat
ci

#16
#The confidence interval tells us there is relatively strong pollster
#effect resulting in a difference of about 5%. Random variability does not seem to explain it.
#Compute a p-value to relay the fact that chance does not explain the observed pollster effect.

# We made an object `res` to summarize the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>% group_by(pollster) %>% 
  summarize(avg = mean(spread), s = sd(spread), N = n()) 

# The variables `estimate` and `se_hat` contain the spread estimates and standard error, respectively.
estimate <- res$avg[2] - res$avg[1]
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])

# Calculate the p-value
(1-pnorm(estimate/se_hat))*2

#17
#Compute the average and standard deviation for each pollster and 
#examine the variability across the averages and how it compares 
#to the variability within the pollsters, summarized by the standard deviation.

# Execute the following lines of code to filter the polling data and calculate the spread
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

# Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. Print the contents of this object to the console.
var<-polls %>% group_by(pollster) %>% summarize(avg=mean(spread),s=sd(spread))
var






















