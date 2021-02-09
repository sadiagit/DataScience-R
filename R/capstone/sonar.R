#install required packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")

#load libraries
library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
library(dplyr)
library(mlbench)
library(rmarkdown)

#load Sonar data
data("Sonar")

#insect first 5 rows
head(Sonar)

#data exploration
dim(Sonar)

unique(Sonar$Class)
any(is.na(Sonar))

mean(Sonar$Class == 'M')

#converting the dataframe to a matrix for PCA
x <- Sonar[, 1:60] %>% as.matrix()


# scale the feature matrix
x_centered <- sweep(x, 2, colMeans(x))
scaled_X <- sweep(x_centered, 2, colSds(x), FUN = "/") 

colMeans(scaled_X)
colSds(scaled_X)

# principal components and summary
pca <- prcomp(scaled_X)

summary(pca)$importance[, 1:30]


#Plot PC1 vs PC2
data.frame(pca$x[,1:2], class=Sonar$Class) %>% 
  ggplot(aes(PC1,PC2, col = class))+
  geom_point() +
  coord_fixed(ratio = 1)

#box plot first 10 PCS

data.frame(pca$x[,1:10], class=Sonar$Class) %>% gather(PCs,Value, -class) %>%
  ggplot(aes(PCs,Value, fill = class))+
  geom_boxplot()

#Model fitting
set.seed(1, sample.kind = "Rounding")

#partitioning to test and train set
test_index <- createDataPartition(Sonar$Class, times = 1, p = 0.2, list = FALSE)
test_x <- scaled_X[test_index,]
test_y <- factor(Sonar$Class[test_index])
train_x <- scaled_X[-test_index,]
train_y <- factor(Sonar$Class[-test_index])

#logistic
train_glm <- train(train_x, train_y,method = "glm")
pred_glm <-  predict(train_glm,  test_x)
acc_glm <- confusionMatrix(pred_glm,test_y)$overall['Accuracy']


# LDA
set.seed(5, sample.kind = "Rounding")

train_lda <- train(train_x, train_y, method = "lda")
pred_lda <-  predict(train_lda,  test_x)
acc_lda <- confusionMatrix(pred_lda,test_y)$overall['Accuracy']


#KNN
set.seed(7, sample.kind = "Rounding")

train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k=c(3:21)))
pred_knn <-  predict(train_knn,  test_x)
acc_knn <- confusionMatrix(pred_knn,test_y)$overall['Accuracy']
train_knn$bestTune


#Random Forest
set.seed(7, sample.kind = "Rounding")

train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry=c(3, 5, 7, 9, 11, 13)), importance=TRUE)
pred_rf <-  predict(train_rf,  test_x)
acc_rf <- confusionMatrix(pred_rf,test_y)$overall['Accuracy']

train_rf$bestTune
varImp(train_rf)


#ensemble
c_preds<- cbind(glm = pred_glm, lda =pred_lda, knn = pred_knn, rf = pred_rf)

ensem <- ifelse(rowMeans(c_preds == 1) > 0.5, 'M', 'R')

acc_ens <- mean(ensem == test_y)

#model fitting on PCA transformed data
sonar_pca <- pca$x[, 1:30]

#create test and train set
test_index <- createDataPartition(Sonar$Class, times = 1, p = 0.2, list = FALSE)
test_x_pca <- sonar_pca[test_index,]
test_y_pca <- factor(Sonar$Class[test_index])
train_x_pca <- sonar_pca[-test_index,]
train_y_pca <- factor(Sonar$Class[-test_index])

set.seed(11, sample.kind = "Rounding")

#logistic
train_glm_pca <- train(train_x_pca, train_y_pca,method = "glm")
pred_glm_pca <-  predict(train_glm_pca,  test_x_pca)
acc_glm_pca <- confusionMatrix(pred_glm_pca,test_y_pca)$overall['Accuracy']

set.seed(13, sample.kind = "Rounding")

#lda
train_lda_pca <- train(train_x_pca, train_y_pca,method = "lda")
pred_lda_pca <-  predict(train_lda_pca,  test_x_pca)
acc_lda_pca <- confusionMatrix(pred_lda_pca,test_y_pca)$overall['Accuracy']

set.seed(15, sample.kind = "Rounding")

#knn
train_knn_pca <- train(train_x_pca, train_y_pca,method = "knn", tuneGrid = data.frame(k=c(3:21)))
pred_knn_pca <-  predict(train_knn_pca,  test_x_pca)
acc_knn_pca <- confusionMatrix(pred_knn_pca,test_y_pca)$overall['Accuracy']

set.seed(17, sample.kind = "Rounding")

#rf
train_rf_pca <- train(train_x_pca, train_y_pca,method = "rf", tuneGrid = data.frame(mtry=c(3,5,7,9)), importance=TRUE)
pred_rf_pca <-  predict(train_rf_pca,  test_x_pca)
acc_rf_pca <- confusionMatrix(pred_rf_pca,test_y_pca)$overall['Accuracy']


#model results -  raw data
data.frame(glm=acc_glm, lda=acc_lda, knn=acc_knn, rf=acc_rf, ensem = acc_ens)

#model results -  pca tranformed data
data.frame(glm_pca=acc_glm_pca, lda_pca=acc_lda_pca, knn_pca=acc_knn_pca, rf_pca=acc_rf_pca)

