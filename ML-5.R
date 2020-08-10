#Section 5: Classification with More than Two Classes and the Caret Package
library(rpart)
library(tidyverse)
library(randomForest)
library(caret)
library(dslabs)
library(titanic)    # loads titanic_train data frame

install.packages("titanic")
install.packages("randomForest")
#5.1: Classification with More than Two Classes 
#assessment

n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") #if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

#1
fit <- rpart(y ~ ., data = dat) 

#2
plot(fit)
plot(fit, margin = 0.1)+text(fit, cex = 0.75)
plot(fit)+text(fit)

#3
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

#4
fit <- randomForest(y~x,data=dat)
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

#5
plot(fit)

#6
fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")
#---------------------------------------------------------------------------------

#5.2 Caret Package

#1
getModelInfo("rpart")

cp=seq(0,0.1,0.01)
data(tissue_gene_expression)
set.seed(1991,sample.kind="Rounding")
trained_rpart<-train(tissue_gene_expression$x,tissue_gene_expression$y,method="rpart",tuneGrid=data.frame(cp=cp))
trained_rpart$results
ggplot(trained_rpart, highlight = TRUE)
trained_rpart$bestTune

#2
control = rpart.control(minsplit = 0)
set.seed(1991,sample.kind="Rounding")
trained_rpart<-train(tissue_gene_expression$x,tissue_gene_expression$y,method="rpart",tuneGrid=data.frame(cp=cp),
                     control=control)
trained_rpart$results
ggplot(trained_rpart, highlight = TRUE)
trained_rpart$bestTune
confusionMatrix(trained_rpart)

#3
plot(trained_rpart$finalModel)+text(trained_rpart$finalModel)

#4
tuneG=data.frame(mtry=seq(50,200,25))
set.seed(1991,sample.kind="Rounding")
fit_rd<-with(tissue_gene_expression, train(x,y,method="rf",tuneGrid=tuneG,nodesize=1))
ggplot(fit_rd)
fit_rd$results

#5
varImp(fit_rd)

#6
imp<-varImp(trained_rpart)
tree_terms <- as.character(unique(trained_rpart$finalModel$frame$var[!(trained_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
importance_rp<-data.frame(vari_imp=imp$importance$Overall,var_name=row.names(imp$importance)) %>%
  mutate(rank=as.integer(rank(desc(vari_imp))))
importance_rp %>% filter(var_name %in% tree_terms)

#given
data_frame(term = rownames(imp$importance), 
           importance = imp$importance$Overall) %>%
  mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
  filter(term %in% tree_terms)

#----------------------------------------------------------------------------------

#5.3 Titanic exercises

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
head(titanic_clean)

#1
?titanic_train
nrow(titanic_train)
set.seed(42,sample.kind = "Rounding")
ind<-createDataPartition(titanic_clean$Survived,,times=1,p=0.2,list=FALSE)
titanic_clean_train<- titanic_clean[-ind,]
titanic_clean_test<- titanic_clean[ind,]
#part a
nrow(titanic_clean_train)
#part b
nrow(titanic_clean_test)
#part c
class(titanic_clean_train$Survived)
mean(titanic_clean_train$Survived==1)

#2s3
p<-0.5
set.seed(3)#,sample.kind = "Rounding")
randompred<-sample(c(0,1),nrow(titanic_clean_test),replace=TRUE)
confusionMatrix(factor(randompred),titanic_clean_test$Survived)
mean(randompred==titanic_clean_test$Survived)

#3 a
mean(titanic_clean_train$Survived[titanic_clean_train$Sex=="female"]==1)
mean(titanic_clean_train$Survived[titanic_clean_train$Sex=="male"]==1)

#3 b
sex_based<-sapply(titanic_clean_test$Sex, function(x){
  ifelse(x=="male",'0','1')
})
confusionMatrix(factor(sex_based),titanic_clean_test$Survived)

#4 a
titanic_clean_train %>% group_by(Pclass) %>%
  summarize(mean=mean(Survived==1))

#4 b
Pclass_based<-ifelse(titanic_clean_test$Pclass %in% c(1),'1','0')
confusionMatrix(factor(Pclass_based),titanic_clean_test$Survived)

#4 c
titanic_clean_train %>% group_by(Pclass,Sex) %>%
  summarize(mean=mean(Survived==1))

#4 d
sex_pclass_based<-ifelse(titanic_clean_test$Pclass %in% c(1,2) & titanic_clean_test$Sex=="female",1,0 )
confusionMatrix(factor(sex_pclass_based),titanic_clean_test$Survived)

#5 a
confusionMatrix(factor(sex_based),titanic_clean_test$Survived)
confusionMatrix(factor(Pclass_based),titanic_clean_test$Survived)
confusionMatrix(factor(sex_pclass_based),titanic_clean_test$Survived)

#5 b
confusionMatrix(factor(sex_based),titanic_clean_test$Survived)

#6
F_meas(factor(sex_based),titanic_clean_test$Survived)
F_meas(factor(Pclass_based),titanic_clean_test$Survived)
F_meas(factor(sex_pclass_based),titanic_clean_test$Survived)

#7
set.seed(1, sample.kind = "Rounding")
titanic_lda<-train(Survived ~ Fare, method = "lda", data = titanic_clean_train)
predicted_lda= predict(titanic_lda,titanic_clean_test)
confusionMatrix(predicted_lda,titanic_clean_test$Survived)

set.seed(1, sample.kind = "Rounding")
titanic_qda<-with(titanic_clean_train,train(Survived ~ Fare, data=titanic_clean_train, method = "qda"))
predicted_qda <- predict(titanic_qda,titanic_clean_test)
confusionMatrix(predicted_qda,titanic_clean_test$Survived)

#8
set.seed(1, sample.kind = "Rounding")
titanic_log<-train(Survived ~ Age, data=titanic_clean_train, method = "glm")
predicted_log <- predict(titanic_log,titanic_clean_test)
confusionMatrix(predicted_log,titanic_clean_test$Survived)

set.seed(1, sample.kind = "Rounding")
titanic_log1<-train(Survived ~ Fare+Sex+Pclass+Age, data=titanic_clean_train, method = "glm")
predicted_log1 <- predict(titanic_log1,titanic_clean_test)
confusionMatrix(predicted_log1,titanic_clean_test$Survived)

set.seed(1, sample.kind = "Rounding")
titanic_log2<-train(Survived ~ ., data=titanic_clean_train, method = "glm")
predicted_log2 <- predict(titanic_log2,titanic_clean_test)
confusionMatrix(predicted_log2,titanic_clean_test$Survived)

#9
k = seq(3, 51, 2)
set.seed(6, sample.kind = "Rounding")
titanic_knn<-train(Survived ~ ., data=titanic_clean_train, method = "knn", tuneGrid=data.frame(k=k))
#a
titanic_knn$bestTune
#b
ggplot(titanic_knn)
#c
predicted_knn <- predict(titanic_knn,titanic_clean_test)
confusionMatrix(predicted_knn,titanic_clean_test$Survived)

#10
control <- trainControl(method = "cv", number = 10, p = .9)
k = seq(3, 51, 2)
set.seed(8, sample.kind = "Rounding")
titanic_knn1<-train(Survived ~ ., data=titanic_clean_train, method = "knn", tuneGrid=data.frame(k=k),trControl=control)
titanic_knn1$bestTune
predicted_knn1 <- predict(titanic_knn1,titanic_clean_test)
confusionMatrix(predicted_knn1,titanic_clean_test$Survived)

#11
cp = seq(0, 0.05, 0.002)
set.seed(10, sample.kind = "Rounding")
titanic_rp<-train(Survived ~ ., data=titanic_clean_train, method = "rpart", tuneGrid=data.frame(cp=cp))

#a
titanic_rp$bestTune
predicted_rp <- predict(titanic_rp,titanic_clean_test)
confusionMatrix(predicted_rp,titanic_clean_test$Survived)

#b
plot(titanic_rp$finalModel)
text(titanic_rp$finalModel)

#c
titanic_rp$finalModel

#12
set.seed(14, sample.kind = "Rounding")
mtry=data.frame(mtry=seq(1,7,1))
titanic_rf<-train(Survived ~ ., data=titanic_clean_train, method = "rf", tuneGrid=mtry, ntree=100)
titanic_rf$bestTune
predicted_rf <- predict(titanic_rf,titanic_clean_test)
confusionMatrix(predicted_rf,titanic_clean_test$Survived)
varImp(titanic_rf)
               
#--------------------------------------------------  