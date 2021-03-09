
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
library(dplyr)
library(mlbench)

# Sonar data from mlbench
data("Sonar")

### Data Exploration

head(Sonar)


### Plots and Principal Component Analysis

dim(Sonar)

# response variable - M and R
unique(Sonar$Class)

# any missing value?
any(is.na(Sonar))

# proportion of M and R in data set
df %>% group_by(class) %>%
  summarise(n=n()) %>%
  mutate( prop=n/sum(n))



### PCA ###

#converting the data frame to a matrix for PCA
x <- Sonar[, 1:60] %>% as.matrix()


# scale the feature matrix
x_centered <- sweep(x, 2, colMeans(x))
scaled_X <- sweep(x_centered, 2, colSds(x), FUN = "/")

colMeans(scaled_X)
colSds(scaled_X)

# principal components
pca <- prcomp(scaled_X)

#Summary of variable importance
summary(pca)$importance[, 1:30]

#Plot first 2 Pcs
data.frame(pca$x[,1:2], class=Sonar$Class) %>%
  ggplot(aes(PC1,PC2, col = class))+
  geom_point() +
  coord_fixed(ratio = 1)+
  stat_ellipse(type="norm", lwd = 1.5)


#Also plot for first 10 PCs:
data.frame(pca$x[,1:10], class=Sonar$Class) %>%
  gather(PCs,Value, -class) %>%
  ggplot(aes(PCs,Value, fill = class))+
  geom_boxplot()


### Modeling
set.seed(1, sample.kind = "Rounding")

#80/20 data split of data set to train and test set respectively
test_index <- createDataPartition(Sonar$Class, times = 1, p = 0.2, list = FALSE)
test_x <- scaled_X[test_index,]
test_y <- factor(Sonar$Class[test_index])
train_x <- scaled_X[-test_index,]
train_y <- factor(Sonar$Class[-test_index])


### Logistic Regression
train_glm <- train(train_x, train_y,method = "glm")
pred_glm <-  predict(train_glm,  test_x)
acc_glm <- confusionMatrix(pred_glm,test_y)$overall['Accuracy']
acc_glm


### LDA
set.seed(5, sample.kind = "Rounding")

train_lda <- train(train_x, train_y, method = "lda")
pred_lda <-  predict(train_lda,  test_x)
acc_lda <- confusionMatrix(pred_lda,test_y)$overall['Accuracy']
acc_lda


### KNN
set.seed(7, sample.kind = "Rounding")

train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k=c(3:21)))
pred_knn <-  predict(train_knn,  test_x)
acc_knn <- confusionMatrix(pred_knn,test_y)$overall['Accuracy']

acc_knn

#best K
train_knn$bestTune

#plot accuracy vs k (# of neighbors)
ggplot(train_knn, highlight = TRUE)


### Random Forest
set.seed(7, sample.kind = "Rounding")

train_rf <- train(train_x, train_y, method = "rf", tuneGrid = data.frame(mtry=c(3, 5, 7, 9, 11, 13)), importance=TRUE)
pred_rf <-  predict(train_rf,  test_x)
acc_rf <- confusionMatrix(pred_rf,test_y)$overall['Accuracy']

#best mtry value
train_rf$bestTune

varImp(train_rf)

#plot accuracy vs random predictors
ggplot(train_rf, highlight = TRUE)


### Ensemble

#combine all above predictions
c_preds<- cbind(glm = pred_glm, lda =pred_lda, knn = pred_knn, rf = pred_rf)

# if more than 50% models predicted M then predict 'M', otherwise predict 'R'
ensem <- ifelse(rowMeans(c_preds == 1) > 0.5, 'M', 'R')

## calc accuracy
acc_ens <- mean(ensem == test_y)

acc_ens

### Results

## Model Result
data.frame("Method"= c('glm','lda','knn','rf','ensemble') ,"Accuracy" =c(acc_glm, acc_lda, acc_knn, acc_rf, acc_ens))

