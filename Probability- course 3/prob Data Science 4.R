#Say you manage a bank that gives out 10,000 loans. The default rate is 0.03 and you lose $200,000 in each foreclosure.
# Assign the number of loans to the variable `n`
n <- 10000

# Assign the loss per foreclosure to the variable `loss_per_foreclosure`
loss_per_foreclosure <- -200000

# Assign the probability of default to the variable `p_default`
p_default <- 0.03

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

#1#Create a random variable S that contains the earnings of 
#your bank. Calculate the total amount of money lost in this scenario.

# Generate a vector called `defaults` that contains the default outcomes of `n` loans
defaults<-sample(c(0,1),n,replace=TRUE,prob=c(1-p_default,p_default))

# Generate `S`, the total amount of money lost across all foreclosures. Print the value to the console.
S<-sum(defaults)*loss_per_foreclosure
S

#2#Run a Monte Carlo simulation with 10,000 outcomes for S, the sum of 
#losses over 10,000 loans. Make a histogram of the results.
# The variable `B` specifies the number of times we want the simulation to run
B <- 10000

# Generate a list of summed losses 'S'. Replicate the code from the previous exercise over 'B' iterations to generate a list of summed losses for 'n' loans.  Ignore any warnings for now.
S<-replicate(B,sum(sample(c(0,1),n,replace=TRUE,prob=c(1-p_default,p_default)))*loss_per_foreclosure)

# Plot a histogram of 'S'.  Ignore any warnings for now.
hist(S)

#3#What is the expected value of S, the sum of losses over 10,000 loans?
#For now, assume a bank makes no money if the loan is paid.
# Calculate the expected loss due to default out of 10,000 loans
n*(p_default*loss_per_foreclosure + (1-p_default)*0) 

#4#What is the standard error of S?
# Compute the standard error of the sum of 10,000 loans
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p_default*(1-p_default))

#5#So far, we've been assuming that we make no money when people pay
#their loans and we lose a lot of money when people default on their
#loans. Assume we give out loans for $180,000. How much money do we
#need to make when people pay their loans so that our net loss is $0?

#In other words, what interest rate do we need to charge in order to not lose money?

# Assign a variable `x` as the total amount necessary to have an expected outcome of $0
x=-loss_per_foreclosure*(p_default/(1-p_default))
x
# Convert `x` to a rate, given that the loan amount is $180,000. Print this value to the console.
x/180000

#6#With the interest rate calculated in the last example, we stilllose money
#50% of the time. What should the interest rate be so that the chance of losing money is 1 in 20?

#In math notation, what should the interest rate be so that Pr(S<0)=0.05?

# Generate a variable `z` using the `qnorm` function
z<-qnorm(0.05)

# Generate a variable `x` using `z`, `p_default`, `loss_per_foreclosure`, and `n`
y<-(z*sqrt(n*p_default*(1-p_default)))
x<- -loss_per_foreclosure * ((n*p_default)-y) / ((n*(1-p_default))+y)

# Convert `x` to an interest rate, given that the loan amount is $180,000. Print this value to the console.
x/180000

#---------------------------------------------------------------------------------