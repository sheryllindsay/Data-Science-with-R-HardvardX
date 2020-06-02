#1
#In a previous exercise, we determined whether or not each poll predicted the
#correct winner for their state in the 2016 U.S. presidential election. Each
#poll was also assigned a grade by the poll aggregator. Now we're going to 
#determine if polls rated A- made better predictions than polls rated C-.
#In this exercise, filter the errors data for just polls with grades A- and
#C-. Calculate the proportion of times each grade of poll predicted the correct winner.

# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals<-errors%>% filter(grade=="A-"| grade=="C-") %>% group_by(grade,hit) %>% summarize(total=length(hit))

# Print the proportion of hits for grade A- polls to the console
totals$total[totals$grade=="A-" & totals$hit==TRUE]/sum(totals$total[totals$grade=="A-"])

# Print the proportion of hits for grade C- polls to the console
totals$total[totals$grade=="C-" & totals$hit==TRUE]/sum(totals$total[totals$grade=="C-"])

#2
#We found that the A- polls predicted the correct winner about 80% of the time 
#in their states and C- polls predicted the correct winner about 86% of the time.
#Use a chi-squared test to determine if these proportions are different.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test<-errors%>% filter(grade=="A-"| grade=="C-") %>% group_by(grade,hit) %>% summarize(total=n()) %>% spread(grade,total)
two_by_two <- tibble(grade=c("FALSE", "TRUE"),'A-'= chisq_test$'A-','C-' =chisq_test$'C-' )
chisq_test <- two_by_two %>%select(-grade) %>%chisq.test()

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

#3
#It doesn't look like the grade A- polls performed significantly differently 
#than the grade C- polls in their states.
#Calculate the odds ratio to determine the magnitude of the difference in
#performance between these two grades of polls.

# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)
chisq_test<-errors%>% filter(grade=="A-"| grade=="C-") %>% group_by(grade,hit) %>% summarize(total=n()) %>% spread(grade,total)
two_by_two <- tibble(grade=c("FALSE", "TRUE"),'A-'= chisq_test$'A-','C-' =chisq_test$'C-' )
# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C<-(two_by_two$'C-'[2]/sum(two_by_two$'C-'))/(two_by_two$'C-'[1]/sum(two_by_two$'C-'))
odds_C

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A<-(two_by_two$'A-'[2]/sum(two_by_two$'A-'))/(two_by_two$'A-'[1]/sum(two_by_two$'A-'))
odds_A

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C


#4
#We did not find meaningful differences between the poll results from grade A- 
#and grade C- polls in this subset of the data, which only contains polls for 
#about a week before the election. Imagine we expanded our analysis to include
#all election polls and we repeat our analysis. In this hypothetical scenario,
#we get that the p-value for the difference in prediction success if 0.0015 and 
#the odds ratio describing the effect size of the performance of grade A- over grade B- polls is 1.07.
#Based on what we learned in the last section, which statement reflects the best interpretation of this result?

# ANS : The p-value is below 0.05, but the odds ratio is very close to 1.
#There is not a scientifically significant difference in performance.

