#Assume the distribution of femaleheights is approximated by a normal distribution
#with a mean of 64 inches and a standard deviation of 3 inches.

# Assign a variable 'female_avg' as the average female height.
female_avg <- 64

# Assign a variable 'female_sd' as the standard deviation for female heights.
female_sd <- 3

#Use pnorm to define the probability that a height will take a value of 6 feet or taller.
1-pnorm(6*12,female_avg,female_sd)

#Use pnorm to define the probability that a randomly chosen woman will be shorter than 67 inches.
pnorm(67,female_avg,female_sd)-pnorm(61,female_avg,female_sd)

#Calculate the probability that a randomly chosen woman will be within 1 SD from the average height.
shorter<-female_avg-female_sd
taller<-female_avg+female_sd
pnorm(taller,female_avg,female_sd)-pnorm(shorter,female_avg,female_sd)

#Determine the height of a man in the 99th percentile, given
#an average height of 69 inches and a standard deviation of 3 inches.
qnorm(0.99,male_avg,male_sd)

#The distribution of IQ scores is approximately normally distributed. 
#The average is 100 and the standard deviation is 15. Suppose you want 
#to know the distribution of the person with the highest IQ in your 
#school district, where 10,000 people are born each year.

#Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation.
#Make a histogram of the highest IQ scores.

# The variable `B` specifies the number of times we want the simulation to run.
B <- 1000
avg<-100
sd<-15
# Use the `set.seed` function to make sure your answer 
#matches the expected result after random number generation.
set.seed(1)

# Create an object called `highestIQ` that contains the highest IQ
#score from each random distribution of 10,000 people.
highestIQ<-replicate(B,{
  max(rnorm(10000,avg,sd))
})
highestIQ
# Make a histogram of the highest IQ scores.
hist(highestIQ)
#--------------------------------------------------------------------------------------

#For the three year period 2016-2018, ACT standardized test scores were
#approximately normally distributed with a mean of 20.9 and standard deviation 
#of 5.7. (Real ACT scores are integers between 1 and 36, 
#but we will ignore this detail and use continuous values instead.)

#First we'll simulate an ACT test score dataset and answer some questions about it.
#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests
#with a mean of 20.9 and standard deviation of 5.7. Save these values as act_scores. You'll be using
#this dataset throughout these four multi-part questions.
mean<-20.9
sd<-5.7
set.seed(16, sample.kind = "Rounding")
act_scores<-rnorm(10000,mean,sd7)
#What is the mean of act_scores?
mean(act_scores)
#What is the standard deviation of act_scores?
sd(act_scores)
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?
sum(act_scores>=36)
#n act_scores, what is the probability of an ACT score greater than 30?
mean(act_scores>=30)
#In act_scores, what is the probability of an ACT score less than or equal to 10?
mean(act_scores<=10)

#Set x equal to the sequence of integers 1 to 36. Use dnorm to determine 
#the value of the probability density function over x given a mean of 20.9 and 
#standard deviation of 5.7; save the result as f_x. Plot x against f_x.
library(ggplot2)
x<-1:36
f_x<-dnorm(x,mean,sd)
plot(x,f_x)

#------------------------------------------------------------------------------------------

#Convert act_scores to Z-scores.you must subtract the mean and then divide by the standard deviation. 
z_scores <- (act_scores - mean(act_scores))/sd(act_scores)
z_scores<-scale(act_scores)
meanz<-mean(z_scores)
sdz<-sd(z_scores)
1-pnorm(2,meanz,sdz)

mean(z_scores>2)
mean(act_scores>(mean+2*sd))

#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
mean+2*sd

#What is the 97.5th percentile of act_scores?
qnorm(0.975,mean,sd)
#-------------------------------------------------------------------------------------

#In this 4-part question, you will write a function to create a CDF for ACT scores.

#Write a function that takes a value and produces the probability of an
#ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.

act_cdf<-function(x){
  mean(act_scores<=x)
}
cdf<-sapply(1:36,act_cdf)
cdf

#What is the minimum integer score such that the probability of that score or lower is at least .95?
min(which(cdf>=0.95))

#Use qnorm() to determine the expected 95th percentile, the value for which the probability of 
#receiving that score or lower is 0.95, given a mean score of 20.9 and standard deviation of 5.7
qnorm(0.95,mean,sd)

#Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01),
#the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
p <- seq(0.01, 0.99, 0.01)
sample_quantiles<-quantile(act_scores,p)
sample_quantiles
#In what percentile is a score of 26?
max(which(sample_quantiles<26))
names(sample_quantiles[max(which(sample_quantiles < 26))])

#create a throretical quantile for the mean and sd wiith same probability and plot it 
#against sample quantiles
theoretical_quantiles<-qnorm(p,mean,sd)
plot(theoretical_quantiles,sample_quantiles)
