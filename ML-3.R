#Section 3: Linear Regression for Prediction, Smoothing, and Working with Matrices
library(caret)
library(dslabs)
library(dplyr)
library(lubridate)
library(tidyverse)
library(purrr)
library(pdftools)
library(broom)
#3.1 Linear Regression for Prediction 

#assesment
set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
n <- 1000
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
head(dat)
head(test)
set.seed(1, sample.kind="Rounding") #if using R 3.6 or later
set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
set.seed(1, sample.kind="Rounding")
rmse <- replicate(10000, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

#2
set.seed(1)    # if R 3.6 or later, set.seed(1, sample.kind="Rounding")
n <- c(100, 500, 1000, 5000, 10000)
res <- sapply(n, function(n){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  rmse <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x, data = train_set)
    y_hat <- predict(fit, newdata = test_set)
    sqrt(mean((y_hat-test_set$y)^2))
  })
  c(avg = mean(rmse), sd = sd(rmse))
})

res

#they asked to apply sapply but i tried that and it didnt work -.- everything else was exactly the same

#4
set.seed(1, sample.kind="Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
set.seed(1, sample.kind="Rounding")
rmse <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x, data = train_set)
  y_hat <- predict(fit, newdata = test_set)
  sqrt(mean((y_hat-test_set$y)^2))
})

mean(rmse)
sd(rmse)

#7
set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
fit <- lm(y ~ x_2, data = train_set)
fit <- lm(y ~ x_1+x_2, data = train_set)

y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

#8
set.seed(1, sample.kind="Rounding")
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y ~ x_1, data = train_set)
fit <- lm(y ~ x_2, data = train_set)
fit <- lm(y ~ x_1+x_2, data = train_set)

y_hat <- predict(fit, newdata = test_set)
sqrt(mean((y_hat-test_set$y)^2))

#------------------------------------------------
#assessment 2
set.seed(2, sample.kind="Rounding") #if you are using R 3.6 or later
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()

#1
dat$train %>% ggplot(aes(x, color = y)) + geom_density()
mu_1 <- seq(0, 3, len=25)
set.seed(2)
answ <- sapply(mu_1, function(d){
  dat <- make_data(mu_1 = d)
  fit <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  y_hat <- ifelse(predict(fit, dat$test) > 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat== dat$test$y)
})
qplot(mu_1, answ)
#----------------------------------------------------

#3.2 Smoothening

#assesment
#1

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  filter(date <= "2018-05-01")

total_days<-diff(range(dat$date))
span<-60/as.numeric(total_days)
fit<-loess(deaths~as.numeric(date),degree=1,span=span,data=dat)

dat %>% mutate(y_hat=predict(fit,as.numeric(date))) %>% ggplot(aes(date,deaths))+
  geom_point(color="grey",alpha=0.5) + geom_line(aes(date,y_hat),color="red") 

head(dat)
dat %>% ggplot(aes(as.numeric(date),deaths))+geom_point() + 
  geom_smooth(color="red", span = span, method.args = list(degree=1))

#2
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

#3
data(mnist_27)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()
qplot(x_2, y, data = mnist_27$train)
fit<-loess(y ~ x_2,degree=1,span=0.15,data=mnist_27$train)

head(mnist_27$train)
mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")

#------------------------------------------------------------------------
#3.3 Working with Matrices
#assessment

#1
x <- matrix(rnorm(100*10), 100, 10)

#2
dim(x)
length(x[1,])
length(x[,1])

#3
x <- x + seq(nrow(x))
x[1,1]
x <- sweep(x, 1, 1:nrow(x),"+")

#4
x <- sweep(x, 2, 1:ncol(x), FUN = "+")

#6
data("mnist_27")
mnist_27$train$image[mnist_27$train$images>50 & mnist_27$train$images<205]
head(mnist_27$train)

mnist <- read_mnist()
length(mnist$train$image[mnist$train$images>50 & mnist$train$images<205])/length(mnist$train$images)

mnist <- read_mnist()
y <- rowMeans(mnist$train$images>50 & mnist$train$images<205)
mean(y)
qplot(as.factor(mnist$train$labels), y, geom = "boxplot")

#----------------------------------------------------------
