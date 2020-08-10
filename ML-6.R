library(caret)
library(dslabs)
library(rpart)
library(tidyverse)
library(randomForest)
library(caret)
library(dslabs)
library(lubridate)
library(tidyverse)

#6.1

#assesment 1
#1
models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

set.seed(1, sample.kind = "Rounding")#` in R 3.6 or later
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

#2
mnist_27$test
length(mnist_27$test$y)
length(models)

pred <- sapply(fits, function(x) 
  predict(x, newdata = mnist_27$test))
dim(pred)
pred

#3
accuracy<-sapply(1:10,function(x){
  mean(pred[,x]==mnist_27$test$y)
})
mean(accuracy)

#4
ensamble<-sapply(1:length(mnist_27$test$y),function(x)
  ifelse(mean(pred[x,]=='7')>0.5,'7','2'))
ensamble    
confusionMatrix(factor(ensamble),mnist_27$test$y) 
ensamble_accuracy<-mean(ensamble==mnist_27$test$y)

#5
models[which(accuracy>ensamble_accuracy)]

#6
acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
mean(acc_hat)

#7
#wrong
acc_ensamble <- sapply(fits, function(fit) ifelse(min(fit$results$Accuracy)>0.8,fit$results$Accuracy,0))
sum(acc_ensamble)/6

#code given:
ind <- acc_hat >= 0.8
votes <- rowMeans(pred[,ind] == "7")
y_hat <- ifelse(votes>=0.5, 7, 2)
mean(y_hat == mnist_27$test$y)

#assesment 2
library(dslabs)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)

#1
tissue_gene_expression %>% ggplot(aes(x[,1], x[,2], color = y)) +  geom_point() #doesn't work
pc <- prcomp(tissue_gene_expression$x)
df<-data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) 
df%>%ggplot(aes(pc_1, pc_2, color = tissue)) +geom_point()

#2
df<-data.frame(pc=pc$x[,1],avg_row=rowMeans(tissue_gene_expression$x),tissue=tissue_gene_expression$y)
cor(df$pc,rowMeans(tissue_gene_expression $x))  
df %>% ggplot(aes(pc,avg_row,color=tissue)) + geom_point()

#3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%ggplot(aes(pc_1, pc_2, color = tissue)) +geom_point()

#4
boxplot(pc$x[,7]~tissue_gene_expression$y)
data.frame(pc<-pc$x[,7],tissue=tissue_gene_expression$y) %>%
  ggplot(aes(tissue,pc)) +
  geom_boxplot()

#given code
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

#5
plot(summary(pc)$importance[3,])
summary(pc)$importance[,3]
#--------------------------------------------------------------------------------------------------
#6.2 Recommendation Systems

data("movielens")
head(movielens)

#1
movielens %>%
  summarize(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId))
#apparently wrong
movie<-movielens %>% group_by(movieId) %>% mutate(total_rating=length(rating))%>% filter(unique(title))
head(movie)
plot(movie$year,sqrt(movie$total_rating))

median_value<-median(movie$total_rating)
xc<-movie %>% filter(total_rating==median_value) %>% group_by(year) %>% mutate(total_count=length(total_rating)) 
xc     

max(xc[3])

xc<-movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year)))
xc$year[which.max(xc$n)]

#Ans:
movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#2
trend<-movielens %>% filter(year>=1993) %>% group_by(movieId) %>% 
  summarize(total_rating=n(),avg_rating=mean(rating),year=2018-first(year),title=title[1]) %>%
  mutate(avg_rating_per_year=total_rating/year) #%>%filter(title=="Forrest Gump")
  
#ans:
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))

#3
plot(trend$avg_rating,trend$avg_rating_per_year)

#4
movielens <- mutate(movielens, date = as_datetime(timestamp))
head(movielens)

#5
movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) + geom_point() +geom_smooth()

#6
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)) %>%
  filter(n >= 1000) %>% 
  ggplot(aes(genres,avg,ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar()


#ans
movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#---------------------------------------------------------------------
#6.3 Regularization

#assesment1
set.seed(1986, sample.kind="Rounding") #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1986, sample.kind="Rounding")` instead
n <- round(2^rnorm(1000, 8, 1))
set.seed(1, sample.kind="Rounding") #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1, sample.kind="Rounding")` instead
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS",1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
top_10_quality<-schools %>% top_n(10, quality) %>% arrange(desc(quality))
set.seed(1, sample.kind="Rounding") #for R 3.5 or earlier
#if using R 3.6 or later, use `set.seed(1, sample.kind="Rounding")` instead
mu <- round(80 + 2*rt(1000, 5))

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% mutate(score = sapply(scores, mean))

#1
top_10_score<-schools %>% top_n(10,score) %>% arrange(desc(score)) %>% select(id,size,score)

#2
median(top_10_score$size)
median(schools$size)

#3
bottom_10_score<-schools %>% top_n(-10,score) %>% arrange(score) %>% select(id,size,score)
median(bottom_10_score$size)

#4
schools %>% ggplot(aes(size,score)) + geom_point() +geom_point(data = filter(schools, rank<=10), col = 2)

#5
overall <- mean(sapply(scores, mean))
alp<-25
schools %>% group_by(id) %>% mutate(b_i= ((score-overall)/(size+alp)*size)) %>%
  mutate(pred = overall +b_i) %>% 
  arrange(desc(b_i)) %>% 
  top_n(10, b_i) 

#6
alp<-10:250
rmse_values<-sapply(alp, function(x){
  pred<-overall+((schools$score-overall)/(schools$size+x)*schools$size)
  sqrt((1/1000)*sum((pred-schools$quality)^2))
  })
alp[which.min(rmse_values)]

#7
schools %>% group_by(id) %>% mutate(b_i= ((score-overall)/(size+alp[which.min(rmse_values)])*size)) %>%
  mutate(pred = overall +b_i) %>% 
  arrange(desc(b_i)) %>% 
  top_n(10, b_i) 

#8
alp<-10:250
rmse_values<-sapply(alp, function(x){
  pred<-((schools$score)/(schools$size+x)*schools$size)
  sqrt((1/1000)*sum((pred-schools$quality)^2))
})
alp[which.min(rmse_values)]

#assesment 2
set.seed(1987, sample.kind="Rounding")
#if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

#1
my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

#2
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#3
s <- svd(y)
names(s)
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

ss_y <- apply(y^2, 2, sum)
ss_yv <- apply((y%*%s$v)^2, 2, sum)
sum(ss_y)

#4
plot(seq(1,ncol(y)), ss_y)
plot(seq(1,ncol(y)), ss_yv)

#5
plot(s$d, sqrt(ss_yv))

#6
sum(s$d[1:3]^2) / sum(s$d^2)

#8
UD <- sweep(s$u, 2, s$d, FUN = "*")
plot(UD[,1], rowMeans(y)) 

#9
my_image(s$v)

#10
plot(s$u[,1], ylim = c(-0.25, 0.25))
plot(s$v[,1], ylim = c(-0.25, 0.25))
with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
my_image(y)

#11
resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#12
resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#13
resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
my_image(cor(resid), zlim = c(-1,1))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

#assessment 3
data("tissue_gene_expression")
#x <- tissue_gene_expression$x
#y <- tissue_gene_expression$y

#1
d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))

#2
h <- hclust(d)
plot(h)

#3
cl <- kmeans(tissue_gene_expression$x, centers = 7)
table(cl$cluster, tissue_gene_expression$y)

#4
library(RColorBrewer)
#install.packages("matrixStats")
sds <- matrixStats::colSds(tissue_gene_expression$x)
ind <- order(sds, decreasing = TRUE)[1:50]
colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
