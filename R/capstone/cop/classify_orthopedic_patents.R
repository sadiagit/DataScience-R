
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(mlbench)) install.packages("mlbench", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(matrixStats)
library(dplyr)
library(corrplot)

# Methods


### Data Exploration

#The data set used in this project can be found in https://www.kaggle.com/uciml/biomechanical-features-of-orthopedic-patients. This data set has 310 rows and 7 columns. There are 6 features and 1 response variable. There are no missing values in the data set.

df <-  read.csv("ortho_patient.csv")
dim(df)
any(is.na(df))

head(df)


#This data set has three categories in response variable. The patients are to be classified into these three categories:

unique(df$class)


#Below is the table that shows the proportion of patients in each class:

prop_her<- mean(df$class == 'Hernia')
prop_normal <- mean(df$class == 'Normal')
prop_spon <- mean(df$class == 'Spondylolisthesis')

data.frame("Class"= c('Hernia','Normal','Spondylolisthesis') ,"Prop" =c(prop_her, prop_normal, prop_spon))


## Plots


#correlation plot
Cor = cor(df[,1:6])

Cor

corrplot(Cor, type="upper", method="ellipse", tl.pos="d")


### Principle Component Analysis

#We will apply PCA to explore the variable importance of each feature. Using the summary function we can see the variability explained by each PC:

#transform to a matrix
x <- df[, 1:6] %>% as.matrix()

# scale and center the feature matrix
x_centered <- sweep(x, 2, colMeans(x))
scaled_X <- sweep(x_centered, 2, colSds(x), FUN = "/")


# principal components
pca <- prcomp(scaled_X)
summary(pca)$importance


#We can plot the first two PCS to see how they explain the variability:

data.frame(pca$x[,1:2], class=df$class) %>%
  ggplot(aes(PC1,PC2, col = class))+
  geom_point() +
  coord_fixed(ratio = 1)

#We can also plot the first 10 PCs:

data.frame(pca$x[,1:6], class=df$class) %>% gather(PCs,Value, -class) %>%
  ggplot(aes(PCs,Value, fill = class))+
  geom_boxplot()



## Modelling

set.seed(1, sample.kind = "Rounding")

#Now We will fit LDA, KNN and Random forest, SVM Linear models to the scaled data set and compare their accuracy.
#First we will split the scaled data set to 80% train set and 20% test set.
test_index <- createDataPartition(df$class, times = 1, p = 0.2, list = FALSE)

#test set 20% of df
test_x <- scaled_X[test_index,]
test_y <- factor(df$class[test_index])

#train set 80% of df
train_x <- scaled_X[-test_index,]
train_y <- factor(df$class[-test_index])


### LDA

set.seed(5, sample.kind = "Rounding")

train_lda <- train(train_x, train_y, method = "lda")

pred_lda <-  predict(train_lda,  test_x)

acc_lda <- confusionMatrix(pred_lda,test_y)$overall['Accuracy']



### K Nearest Neighbours

set.seed(7, sample.kind = "Rounding")

#For KNN, I am using tuning parameter k from 15 to 40 and the default cross validation is performed by taking 25 bootstrap samples comprised of 25% of the observations
train_knn <- train(train_x, train_y, method = "knn", tuneGrid = data.frame(k=c(15:40,2)))
pred_knn <-  predict(train_knn,  test_x)
acc_knn <- confusionMatrix(pred_knn,test_y)$overall['Accuracy']

train_knn$bestTune

#plot KNN
ggplot(train_knn, highlight = TRUE)


### SVM Linear Model

set.seed(20, sample.kind = "Rounding")

#For SVM Linear model, I have used tuning parameter C from 1 to 10 and 10 fold cross validation.
train_control <- trainControl(method="repeatedcv", number=10, repeats=3)

train_svm <- train(train_x, train_y, method = "svmLinear",
                   tuneGrid = data.frame(C=c(1:10,2)), trControl = train_control)

pred_svm <-  predict(train_svm,  test_x)

acc_svm <- confusionMatrix(pred_svm,test_y)$overall['Accuracy']

#best tune
train_svm$bestTune

#plot
ggplot(train_svm, highlight = TRUE)



### Random Forest

set.seed(9, sample.kind = "Rounding")

#For Random forest, tune grid parameter is mtry with values from 3 to 13.
train_rf <- train(train_x, train_y, method = "rf",
                  tuneGrid = data.frame(mtry=c(3, 5, 7, 9, 11, 13)), importance=TRUE)

pred_rf <-  predict(train_rf,  test_x)

acc_rf <- confusionMatrix(pred_rf,test_y)$overall['Accuracy']

#plot
ggplot(train_rf, highlight = TRUE)

varImp(train_rf)



# Results

data.frame("Method"= c('lda','knn','rf','svm_linear') ,"Accuracy" =c( acc_lda, acc_knn, acc_rf, acc_svm))





