#1
#For the following exercises, we will use actual poll data from the 2016 election.
#The exercises will contain pre-loaded data from the dslabs package.

library(dslabs)
data("polls_us_election_2016")
We will use all the national polls that ended within a few weeks before the election.

#Assume there are only two candidates and construct a 95% confidence interval for the election night proportion p.
# Load the data
data(polls_us_election_2016)
summary(polls_us_election_2016)
# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls<-polls_us_election_2016 %>% filter(state=="U.S." & enddate>="2016-10-31")
polls
# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N<-polls$samplesize[1]
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat<-mean(polls$rawpoll_clinton[1]/100)
X_hat
# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat<-sqrt((X_hat*(1-X_hat))/N)
se_hat
# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-c(X_hat-qnorm(0.975)*se_hat,X_hat+qnorm(0.975)*se_hat)
ci

#2
#Create a new object called pollster_results that contains
#the pollster's name, the end date of the poll, the proportion 
#of voters who declared a vote for Clinton, the standard error
#of this estimate, and the lower and upper bounds of the confidence interval for the estimate.

# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
pollster_results <- polls %>% 
  mutate(X_hat = polls$rawpoll_clinton/100, se_hat = sqrt(X_hat*(1-X_hat)/samplesize), lower = X_hat - qnorm(0.975)*se_hat, upper = X_hat + qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, X_hat, se_hat, lower, upper)

#3
#The final tally for the popular vote was Clinton 48.2% and Trump 
#46.1%. Add a column called hit to pollster_results that states if 
#the confidence interval included the true proportion p=0.482 or not.
#What proportion of confidence intervals included p?

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
p=0.482
# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit <- pollster_results %>% mutate(hit=(p>=lower & p<=upper)) %>% summarize(mean(hit))

#4
#A much smaller proportion of the polls than expected produce confidence intervals
#containing p. Notice that most polls that fail to include p are underestimating. 
#The rationale for this is that undecided voters historically divide evenly between
#the two main candidates on election day.
#In this case, it is more informative to estimate the spread or the difference 
#between the proportion of two candidates d, or 0.482???0.461=0.021 for this election.
#Assume that there are only two parties and that d=2p???1. Construct a 95% 
#confidence interval for difference in proportions on election night.

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") %>% mutate(d_hat=(rawpoll_clinton-rawpoll_trump)/100)

head(polls)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N<-polls$samplesize[1]
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat<-(polls$d_hat[1])

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat<-(d_hat+1)/2

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat<-2*sqrt(X_hat*(1-X_hat)/N)


# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<-d_hat+c(-qnorm(0.975),qnorm(0.975))*se_hat
ci

#6
#Create a new object called pollster_results that
#contains the pollster's name, the end date of the 
#poll, the difference in the proportion of voters who 
#declared a vote either, and the lower and upper bounds 
#of the confidence interval for the estimate.

# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
poll<-polls %>% mutate(d_hat=(rawpoll_clinton-rawpoll_trump)/100,X_hat=(d_hat+1)/2,se_hat=2*sqrt(X_hat*(1-X_hat)/samplesize),lower=d_hat-(qnorm(0.975)*se_hat),upper=d_hat+(qnorm(0.975)*se_hat))
pollster_results<- poll %>% select(pollster, enddate, d_hat, lower, upper)

#7
#What proportion of confidence intervals for the difference between the proportion of voters included 
#d, the actual difference in election day?

# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
avg_hit<- pollster_results %>% mutate(hit=(0.021>=lower & 0.021<=upper)) %>% summarize(mean(hit))

#8
#Although the proportion of confidence intervals
#that include the actual difference between the proportion
#of voters increases substantially, it is still lower that
#0.95. In the next chapter, we learn the reason for this.
#To motivate our next exercises, calculate the difference
#between each poll's estimate d¯ and the actual d=0.021. Stratify 
#this difference, or error, by pollster in a plot.

# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
d=0.021
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
data <- polls %>% mutate(error=((rawpoll_clinton-rawpoll_trump)/100)-d)
data %>% ggplot(aes(x = pollster, y = error)) +geom_point() +theme(axis.text.x = element_text(angle = 90, hjust = 1))

#9
#Remake the plot you made for the previous exercise, but only for pollsters that took five or more polls.
#You can use dplyr tools group_by and n to group data by a variable of 
#interest and then count the number of observations in the groups. The 
#function filter filters data piped into it by your specified condition.
#For example:
  
  data %>% group_by(variable_for_grouping) 
%>% filter(n() >= 5)
  
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
d<-0.021
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
data <- polls %>% mutate(error=((rawpoll_clinton-rawpoll_trump)/100)-d) %>% group_by(pollster) %>% filter(n()>=5) 
data %>% ggplot(aes(x = pollster, y = error)) +geom_point() 










































































