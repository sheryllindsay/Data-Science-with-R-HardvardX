# Section 4
#Text Mining

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
#load dataset
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")


trump_tweets %>%head
head(trump_tweets)
names(trump_tweets)

trump_tweets %>% select(text) %>% head
trump_tweets %>% count(source) %>% arrange(desc(n))

campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

head(campaign_tweets)
#visualization
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

#text as data

install.packages("tidytext")
library(tidytext)

example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

#pattern to include characters like @ and #
pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#ignoring websites
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

#extracting words from all tweets
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern)

tweet_words

#frequently used words
tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

#filtering out stop_words
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word )
tweet_words
names(tweet_words)

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

#removing numbers and ' at the start
tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

tweet_words

#most tweeted between android and iphone
android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)

#filtering uncommon words
#0.5 cus some words have 0 occurance
android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)


#Sentiment Analysis
sentiments

#divides words to negative and positive
get_sentiments("bing")

install.packages("textdata")
#teata is a requirement and this scores words from -5 to 5
get_sentiments("afinn")

#some other lexicons used
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

?sentiments

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

nrc

#inner join some common words with sentimental analysis
tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

#getting sentiment count for android and iphone users
sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

install.packages("boom")
library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or

#graphical representation of sentiments
log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 

#words that show negative feelings
android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))
#graph representation
android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
#------------------------------------------------------------
# section 4 assesment
library(lubridate)
library(tidyr)
library(scales)

#3
data(brexit_polls)
head(brexit_polls)
nrow(brexit_polls%>% filter(month(startdate)=='4'))
x<-round_date(brexit_polls$enddate,unit="week")
sum(x=="2016-06-12")
?round_date
sum(week(x)==week(2016-06-12))

sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

#4
table(weekdays(brexit_polls$enddate))

#5
data(movielens)
head(movielens)
x<-as_datetime(movielens$timestamp)
y<-year(x)
max(table(y))

y<-hour(x)
which(table(y)==max(table(y)))

#assesment 2
library(tidyverse)
install.packages("gutenbergr")
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata

#6
x<-which(str_detect(gutenberg_metadata$title,"Pride and Prejudice"))
length(x)

#7  function finds english books and remives duplicates
gutenberg_works(str_detect(gutenberg_metadata$title,"^Pride and Prejudice$"))

#8
temp<-gutenberg_download(1342)
head(temp)
temp1 <-temp %>% unnest_tokens(word,text)
nrow(temp1)

#9
head(stop_words)
head(temp1)
temp1<-temp1 %>% filter(!word %in% stop_words$word)
nrow(temp1)

#10
temp2<-temp %>% unnest_tokens(word,text,token="regex",pattern="\\d") %>% filter(!word %in% stop_words$word)
nrow(temp2)

temp1 %>% filter(!str_detect(word,"\\d")) %>% nrow()

#11
x<-temp1 %>% filter(!str_detect(word,"\\d")) 
head(x)

x %>% count(word) %>% filter(n>100) %>% nrow()

x %>% count(word) %>% arrange(desc(n))

#12
afinn <- get_sentiments("afinn")
head(afinn)
afinn_sentiments<- inner_join(afinn,x,by="word")
head(afinn_sentiments)
nrow(afinn_sentiments)

afinn_sentiments %>% filter(is.na(value)) %>% nrow()

class(afinn_sentiments$value)
afinn_sentiments %>% filter(value>0) %>% nrow() /afinn_sentiments %>% nrow()

afinn_sentiments %>% filter(value=="4") %>% nrow()
