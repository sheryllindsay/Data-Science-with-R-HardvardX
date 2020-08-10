#Section 4: Distance, Knn, Cross-validation, and Generative Models
#4.1 Nearest Neighbours
library(dslabs)
library(tidyverse)
library(caret)
library(genefilter)

#assesment 1

#1
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

d <- dist(tissue_gene_expression$x)
d

#2
head(tissue_gene_expression$x)
dist(tissue_gene_expression$x[1,],tissue_gene_expression$x[2,])
as.matrix(d)[1:2,1:2]
as.matrix(d)[39:40,39:40]
as.matrix(d)[73:74,73:74]
num1<-c(1,2,39,40,73,74)
as.matrix(d)[num1,num1]

#3
image(as.matrix(d))

#assessment 2

#1
head(heights)
set.seed(1, sample.kind="Rounding")

k<-seq(1, 101, 3)
test_index <- createDataPartition(heights$height, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)
set.seed(1, sample.kind="Rounding")
knn_vals<-sapply(k, function(k){
  knn_fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat_knn <- predict(knn_fit, test_set, type = "class") %>% factor(levels = levels(train_set$sex))
  F_meas(y_hat_knn,reference=test_set$sex)
})
max(knn_vals)

knn_vals[which.max(knn_vals)]
k[which.max(knn_vals)]

# I dont get the difference
max(knn_vals)
k

set.seed(1)
test_index <- createDataPartition(heights$sex, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]     

ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class") %>% 
    factor(levels = levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]

#2
data("tissue_gene_expression")
set.seed(1)
train_index<- createDataPartition(tissue_gene_expression$y,times=1, p=0.5, list= FALSE)
trainx<-tissue_gene_expression$x[-train_index,]
trainy<-tissue_gene_expression$y[-train_index]
testx<-tissue_gene_expression$x[train_index,]
testy<-tissue_gene_expression$y[train_index]
k =c( 1, 3, 5, 7, 9, 11 )
sapply(k,function(k){
  fit<-knn3(trainx,trainy, k=k)
  y_hat<-predict(fit,newdata=data.frame(testx),type="class")
  mean(y_hat==testy)
})

#----------------------------------------------------------------------------

#4.2 Cross-Validation

set.seed(1996) #if you are using R 3.5 or earlier
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

#1
fit <- train(x_subset, y,method="glm")
fit$results

#2
install.packages("BiocManager")
BiocManager::install("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
head(tt)

#3
ind<-which(pvals<=0.01)
length(ind)

#4
x_subset <- x[,ind]
fit <- train(x_subset, y,method="glm")
fit$results

#5
k = seq(101, 301, 25)
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 5)))
ggplot(fit)

#7
k = seq(1,7,2)
fit <- train(x, y, method = "knn", tuneGrid = data.frame(k = k))
ggplot(fit)
fit$results

#assesment 2
data(mnist_27)
set.seed(1995, sample.kind="Rounding") # if R 3.6 or later, set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

#1
sum(indexes$Resample01==3)
sum(indexes$Resample01==4)
sum(indexes$Resample01==7)

#2
li<-seq(1,10,1)
x<-sapply(li, function(ind){
  sum(indexes[[ind]] == 3)
})
sum(x)

#3
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
m_c<-replicate(10000,{
  y <- rnorm(100, 0, 1)
  quantile(y,0.75)
})
mean(m_c)
sd(m_c)

#4
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
y_boot<-createResample(y,10)
y_boot[[1]]

li<-seq(1,10,1)
x<-sapply(li,function(li){
  quantile(y[y_boot[[li]]],0.75)
})
mean(x)
sd(x)

#5
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)
set.seed(1, sample.kind="Rounding")
y_boot<-createResample(y,10000)
y_boot[[1]]

li<-seq(1,10000,1)
x<-sapply(li,function(li){
  quantile(y[y_boot[[li]]],0.75)
})
mean(x)
sd(x)
#--------------------------------------------------------------

#4.3 Generative Models
library(caret)
library(tidyverse)
data("tissue_gene_expression")

#run-again point
set.seed(1993, sample.kind="Rounding")
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#1
trained<-train(x,y,method="lda")
trained$results

#2
p1<-trained$finalModel$means[1,]
p2<-trained$finalModel$means[2,]
p3<-names(p1)
ps<-data.frame(p1=p1,p2=p2,p3=p3)
ps %>% ggplot(aes(p1,p2,color=p3)) + geom_point(alpha=2,size=4) +geom_text(aes(label=p3))

#code used
t(trained_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(cerebellum, hippocampus, label = predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()+coord_flip()

#3
data("tissue_gene_expression")

#run-again point

trained_qda<-train(x,y,method="qda")
trained_qda$results

#4
p1<-trained_qda$finalModel$means[1,]
p2<-trained_qda$finalModel$means[2,]
p3<-names(p1)
ps<-data.frame(p1=p1,p2=p2,p3=p3)
ps %>% ggplot(aes(p1,p2,color=p3)) + geom_point(alpha=2,size=4) +geom_text(aes(label=p3))

#5

#run again point

trained_lda<-train(x,y,method="lda",preProcess="center")
trained_lda$results

p1<-trained_lda$finalModel$means[1,]
p2<-trained_lda$finalModel$means[2,]
p3<-names(p1)
ps<-data.frame(p1=p1,p2=p2,p3=p3)
ps %>% ggplot(aes(p1,p2,color=p3)) + geom_point(alpha=2,size=4)+geom_text(aes(label=p3))

#used code
fit_lda <- train(x, y, method = "lda", preProcess = "center")
fit_lda$results["Accuracy"]
t(fit_lda$finalModel$means) %>% data.frame() %>%
  mutate(predictor_name = rownames(.)) %>%
  ggplot(aes(predictor_name,cerebellum)) +
  geom_point() +
  coord_flip() # we have to change cerebellum to hipocampus

d <- apply(fit_lda$finalModel$means, 2, diff)
ind <- order(abs(d), decreasing = TRUE)[1:2]
plot(x[, ind], col = y)

#6
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") #if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

trained_lda<-train(x,y,method="lda")
trained_lda$results
