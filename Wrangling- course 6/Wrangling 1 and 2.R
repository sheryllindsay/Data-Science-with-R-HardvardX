# ASSESMENTS

#Section 1

library("dslabs")
data("murders")
getwd()
setwd()
ls()

#14
library("tidyverse")
url<-"http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_table(url)
read_tsv(url)

#15-16
url <- "http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
heady<-read_csv(url,col_names = FALSE)
nrowread_lines(url, n_max = 3)     
nrow(heady)
ncol(heady)
ncol(read_table(url))
-------------------------------------------------------------------------------------------
  
#section 2
  # 2.1
  
#3
system.file("extdata", package = "dslabs")
path<-"C:/Users/shery/Documents/R/win-library/4.0/dslabs/extdata"
files <- list.files(path)
files

#9
x<-co2
head(x)
     
#10
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide
co2_tidy <- gather(co2_wide,month,co2,-year)
co2_tidy

#11
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

#12
library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
dat
dat_tidy <- spread(dat, gender, admitted)
dat_tidy

#13
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
tmp2 <- unite(tmp, column_name, c(key, gender))
tmp2
#-------------------------------------------------------------------
  #2.2 

#questions 5-7
install.packages("Lahman")
library(Lahman)
data("Batting")
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)
top %>% as_tibble()
Master %>% as_tibble()

#5
top_names <- top %>% left_join(Master) %>% select(playerID, nameFirst, nameLast, HR)

#6
top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names)%>%
  select(nameFirst, nameLast, teamID, HR, salary)

#7
x<-AwardsPlayers %>% filter(yearID==2016)
head(x)
length(intersect(x$playerID,top_names$playerID))
length(setdiff(x$playerID,top_names$playerID))

#-------------------------------------------------------------------------
  #2.3 Web Scraping
install.packages("rvest")
library("rvest")
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 
x<-get_recipe("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
y<-get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

#Assesment 

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes<-html_nodes(h,"table")
html_text(nodes[[8]])
html_table(nodes[[8]])

#1
html_table(nodes[[1]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

#2
length(nodes)
html_table(nodes[[23]])
html_table(nodes[[22]])
html_table(nodes[[21]])

#3
x<-html_table(nodes[[10]])
y<-html_table(nodes[[18]])
names(y)<-y[1,]
y<-y[-1,]
x<-x[,1:3]
nrow(full_join(x,y,by="Team"))

#4-5
library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
h<-read_html(url)
tab<-html_nodes(h,"table")
length(tab)

#5
find_table<-function(n){
  temp<-html_table(tab[[5]],fill=TRUE)
  if(ncol(temp)==9 & colnames(temp)[1]=="Date(s) conducted" ){print(n)}
}

sapply(1:39,find_table)

colnames(temp)[1]
ncol(temp)

#--------------------------------------------------------------------

