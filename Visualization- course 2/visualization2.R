library(dslabs)
data(heights)

#Use summary() on the heights$height variable to find the quartiles:
summary(heights$height)
p <- seq(0.01, 0.99, 0.01)  #when used with this probability gives percentiles 
percentiles <- quantile(heights$height, p)
x<-quantile(heights$height,0.25)
x
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]
names(percentiles)

#qnorm gives the inverse of cdf
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles

#finding index value
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
z

#ploting quantiles against qnorm to see if they match
observed_quantiles <- quantile(z, p)
observed_quantiles
theoretical_quantiles <- qnorm(p)
theoretical_quantiles
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

#loading the gapminder dataset
library(tidyverse)
library(dslabs)
data(gapminder)

#Life Expectancy increase for West countries from 2010 to 2015 and population greater than 1 million
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")