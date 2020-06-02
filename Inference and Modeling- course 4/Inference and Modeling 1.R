#1
#Suppose you poll a population in which a proportion p 
#of voters are Democrats and 1???p are Republicans. Your 
#sample size is N=25. Consider the random variable S, 
#which is the total number of Democrats in your sample.
#What is the expected value of this random variable S?

# ANS : E(S)=25p

#2
#Again, consider the random variable S, which is the total 
#number of Democrats in your sample of 25 voters. 
#The variable p describes the proportion of Democrats in the 
#sample, whereas 1???p describes the proportion of Republicans.
#What is the standard error of S?

#ANS : SE(S)=???(25p(1???p))

#3
#Consider the random variable S/N, which is equivalent to the 
#sample average that we have been denoting as X¯. The variable 
#N represents the sample size and p is the proportion of Democrats in the population.
#What is the expected value of X¯?

# ANS: E(X¯)=p

#4
#What is the standard error of the sample average, X¯?
#The variable N represents the sample size and p is the proportion of Democrats in the population.

#ANS:SE(X¯)=???(p(1???p)/N)

#5
#Write a line of code that calculates the standard error se 
#of a sample average when you poll 25 people in the population.
#Generate a sequence of 100 proportions of Democrats p that 
#vary from 0 (no Democrats) to 1 (all Democrats).
#Plot se versus p for the 100 different proportions.
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p<-seq(0,1,length=100)

# Create a variable `se` that contains the standard error of each sample average
se<-sqrt((p*(1-p))/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(p,se)

#6
#Using the same code as in the previous exercise, create a 
#for-loop that generates three plots of p versus se when the 
#sample sizes equal N=25, N=100, and N=1000.
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every 
#value of `p` for each of the three samples sizes `N` in the vector 
#`sample_sizes`. Plot the three graphs, using the `ylim` argument to
#standardize the y-axis across all three plots.

for(N in sample_sizes){
  se<-sqrt(((p*(1-p))/N))
  plot(p,se,ylim=c(0,0.1))
}


#7
#Our estimate for the difference in proportions of Democrats and Republicans is d=X¯???(1???X¯).
#Which derivation correctly uses the rules we learned about sums 
#of random variables and scaled random variables to derive the expected value of d?

#ANS : E[X¯???(1???X¯)]=E[2X¯???1] =2E[X¯]???1 =2p???1 =p???(1???p)

#8
#Our estimate for the difference in proportions of Democrats and Republicans is d=X¯???(1???X¯).
#Which derivation correctly uses the rules we learned about sums of random variables 
#and scaled random variables to derive the standard error of d?

#ANS : SE[X¯???(1???X¯)]=SE[2X¯???1] =2SE[X¯] =???(2p(1???p)/N)

#9
#Say the actual proportion of Democratic voters is p=0.45. In this case,
#the Republican party is winning by a relatively large margin of d=???0.1,
#or a 10% margin of victory. What is the standard error of the spread 2X¯???1 in this case?

# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2*sqrt((p*(1-p)/N))

#10
#So far we have said that the difference between the proportion of Democratic
#voters and Republican voters is about 10% and that the standard error of this
#spread is about 0.2 when N=25. Select the statement that explains why this
#sample size is sufficient or not.

# ANS: This sample size is too small because the standard error is larger than the spread.
