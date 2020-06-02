#1
#For each poll in the polling data set, use the CLT to create a 95% confidence interval for the 
#spread. Create a new table called cis that contains columns for the lower and upper limits of the confidence intervals.

# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# Create an object called `cis` that has the columns indicated in the instructions
cis<-polls %>% mutate(X_hat=(spread+1)/2,se=sqrt(X_hat*(1-X_hat)/samplesize) ,lower=(2*X_hat-1)-qnorm(0.975)*2*se,upper=(2*X_hat-1)+qnorm(0.975)*2*se) %>% select(state, startdate, enddate, pollster, grade, spread, lower, upper)

#2
#You can add the final result to the cis table you just created using
#the left_join function as shown in the sample code.
#Now determine how often the 95% confidence interval includes the actual result.

# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. Print this object to the console.
p_hits<-ci_data %>% mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper,TRUE,FALSE)) %>% summarize(mean(hit))
p_hits

#3
#Now find the proportion of hits for each pollster. Show only pollsters with at
#least 5 polls and order them from best to worst. Show the number of polls
#conducted by each pollster and the FiveThirtyEight grade of each pollster.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits<-ci_data %>% mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper,TRUE,FALSE)) %>% group_by(pollster) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n(),grade=grade[1]) %>% arrange(desc(proportion_hits))
p_hits

#4
#Repeat the previous exercise, but instead of pollster, stratify by state. Here we can't show grades.

# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.

p_hits<-ci_data %>% mutate(hit=ifelse(actual_spread>=lower & actual_spread<=upper,TRUE,FALSE)) %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n()) %>% arrange(desc(proportion_hits))
p_hits

#5
#Make a barplot based on the result from the previous exercise.
# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat="identity")+coord_flip()

#6
#Even if a forecaster's confidence interval is incorrect, the overall predictions will do better
#if they correctly called the right winner.
#Add two columns to the cis table by computing, for each poll, the difference between the
#predicted spread and the actual spread, and define a column hit that is true if the signs are the same.
# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors<-cis %>% mutate(error=spread-actual_spread,hit=(sign(actual_spread)==sign(spread)))
#?sign
# Examine the last 6 rows of `errors`
tail(errors)

#7
#Create an object called p_hits that contains the proportion of instances 
#when the sign of the actual spread matches the predicted spread for states with 5 or more polls.
#Make a barplot based on the result from the previous exercise that shows 
#the proportion of times the sign of the spread matched the actual result for the data in p_hits.

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits<-errors %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n()) %>% arrange(proportion_hits)
# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(state,proportion_hits)) + geom_bar(stat="identity") + coord_flip()

#8
#Make a histogram of the errors. What is the median of these errors?

# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

#9
#Create a boxplot to examine if the bias was general to all states or if it 
#affected some states differently. Filter the data to include only pollsters with grades B+ or higher.
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+")) %>% arrange(error) %>% ggplot(aes(state,error)) + geom_boxplot()+geom_point() 

#10
#Some of these states only have a few polls. Repeat
#the previous exercise to plot the errors for each state, but only include states with five good polls or more.


# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% filter(grade %in% c("A+","A","A-","B+")) %>% group_by(state) %>% filter(n()>4) %>% ungroup() %>% arrange(error) %>% ggplot(aes(state,error)) + geom_boxplot()+geom_point() 

--------------------------------------------------------------------------------------------------
  
  
#1
#We know that, with a normal distribution, only 5% of values are more than 2 standard deviations away from the mean.
#Calculate the probability of seeing t-distributed random variables being more than 2 in
#absolute value when the degrees of freedom are 3.

  1-pt(2,3)+pt(-2,3)

#2
#Now use sapply to compute the same probability for degrees of freedom from 3 to 50.
#Make a plot and notice when this probability converges to the normal distribution's 5%.

# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df<-3:50

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func<-function(n){
  1-pt(2,n)+pt(-2,n)
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs<-sapply(df,pt_func)

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(df,probs)

#3
#Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice what happens 
#to the proportion of hits.

# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res<-replicate(B,{
  temp<-sample(x,N,replace=TRUE)
  interval<-mean(temp)+c(-qnorm(0.975),qnorm(0.975))*sd(temp/sqrt(N))
  between(mu,interval[1],interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

#4
#What are the proportion of 95% confidence intervals that span the actual mean height now?

# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res<-replicate(B,{
  X<-sample(x,N,replace=TRUE)
  interval<-mean(X) + c(-1,1)*qt(0.975, N-1)*sd(X)/sqrt(N)
  between(mu,interval[1],interval[2])
})

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)

#5
#Why did the t-distribution confidence intervals work so much better?

# ANS : The t-distribution takes the variability into account and generates larger confidence intervals.







