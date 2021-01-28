Titanic
================
Sadia Boksh
15/12/2020

# Introduction

The Titanic was a British ocean liner that struck an iceberg and sunk on
its maiden voyage in 1912 from the United Kingdom to New York. More than
1,500 of the estimated 2,224 passengers and crew died in the accident,
making this one of the largest maritime disasters ever outside of war.
The ship carried a wide range of passengers of all ages and both
genders, from luxury travelers in first-class to immigrants in the lower
classes. However, not all passengers were equally likely to survive the
accident. We will use real data about a selection of 891 passengers to
predict which passengers survived.

# Data

``` r
# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
    mutate(Survived = factor(Survived),
           Embarked = factor(Embarked),
           Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
           FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

head(titanic_clean)
```

    ##   Survived    Sex Pclass Age  Fare SibSp Parch FamilySize Embarked
    ## 1        0   male      3  22  7.25     1     0          2        S
    ## 2        1 female      1  38 71.28     1     0          2        C
    ## 3        1 female      3  26  7.92     0     0          1        S
    ## 4        1 female      1  35 53.10     1     0          2        S
    ## 5        0   male      3  35  8.05     0     0          1        S
    ## 6        0   male      3  28  8.46     0     0          1        Q

# Training and test sets

Now we will split titanic\_clean into test and training sets We will use
the caret package to create a 20% data partition based on the Survived
column and assign the 20% partition to test\_set and the remaining 80%
partition to train\_set.

``` r
set.seed(42, sample.kind = "Rounding")
ind <- createDataPartition(titanic_clean$Survived, times = 1,p=0.2, list=FALSE)
train_set <- titanic_clean[-ind,]
test_set <- titanic_clean[ind,]

nrow(train_set)
```

    ## [1] 712

``` r
nrow(test_set)
```

    ## [1] 179

``` r
mean(train_set$Survived == 1)
```

    ## [1] 0.383

The simplest prediction method is randomly guessing the outcome without
using additional predictors. These methods will help us determine
whether our machine learning algorithm performs better than chance. For
each individual in the test set, we will randomly guess whether that
person survived or not by sampling from the vector c(0,1).

``` r
set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)

# accuracy
mean(guess == test_set$Survived)
```

    ## [1] 0.475

# Predicting survival by sex

First we will determine whether members of a given sex were more likely
to survive or die. Proportion of female and male survived in the
training set:

``` r
#female
train_set %>% filter(Sex == "female") %>% group_by(Survived) %>% summarize(n=n()) %>% mutate(prop = n/sum(n))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   Survived     n  prop
    ##   <fct>    <int> <dbl>
    ## 1 0           67 0.269
    ## 2 1          182 0.731

``` r
#male
train_set %>% filter(Sex == "male") %>% group_by(Survived) %>% summarize(n=n()) %>% mutate(prop = n/sum(n))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 2 x 3
    ##   Survived     n  prop
    ##   <fct>    <int> <dbl>
    ## 1 0          372 0.803
    ## 2 1           91 0.197

Now we will predict survival using sex on the test set: if the survival
rate for a sex is over 0.5, predict survival for all individuals of that
sex, and predict death if the survival rate for a sex is under 0.5.

``` r
ts <- test_set %>% group_by(Sex) %>% mutate(survival_rate = mean(Survived == 1))
y_hat_Sex <- ifelse(ts$survival_rate > 0.5, 1,0)

#accuracy 
mean(y_hat_Sex == test_set$Survived)
```

    ## [1] 0.821

# Predicting survival by passenger class

``` r
train_set %>% group_by(Pclass) %>% summarize(pclass_s_rate = mean(Survived == 1))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

    ## # A tibble: 3 x 2
    ##   Pclass pclass_s_rate
    ##    <int>         <dbl>
    ## 1      1         0.619
    ## 2      2         0.5  
    ## 3      3         0.242

Class 1 passengers most likelt to survive.

Now we will predict survival if the survival rate for a class is over
0.5, otherwise predict death.

Accuracy shown below:

``` r
y_hat_by_pclass <- ifelse(test_set$Pclass == 1, 1,0)
mean(y_hat_by_pclass == test_set$Survived)
```

    ## [1] 0.704

Using the training set to group passengers by both sex and passenger
class. It looks like females in 1st and 2nd classes are most likely to
survive.

``` r
train_set %>% group_by(Sex, Pclass) %>% summarise(r = mean(Survived == 1))
```

    ## `summarise()` regrouping output by 'Sex' (override with `.groups` argument)

    ## # A tibble: 6 x 3
    ## # Groups:   Sex [2]
    ##   Sex    Pclass     r
    ##   <chr>   <int> <dbl>
    ## 1 female      1 0.957
    ## 2 female      2 0.919
    ## 3 female      3 0.5  
    ## 4 male        1 0.384
    ## 5 male        2 0.183
    ## 6 male        3 0.135

Now we will predict survival if the survival rate for a sex/class
combination is over 0.5, otherwise predict death. Accuracy as below:

``` r
y_hat_sex_class <- ifelse((test_set$Sex == "female" & test_set$Pclass %in% c(1,2)), 1,0)
mean(y_hat_sex_class == test_set$Survived)
```

    ## [1] 0.821

# Confusion matrix

## Sex based model

``` r
confusionMatrix(factor(y_hat_Sex), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 96 18
    ##          1 14 51
    ##                                         
    ##                Accuracy : 0.821         
    ##                  95% CI : (0.757, 0.874)
    ##     No Information Rate : 0.615         
    ##     P-Value [Acc > NIR] : 1.72e-09      
    ##                                         
    ##                   Kappa : 0.619         
    ##                                         
    ##  Mcnemar's Test P-Value : 0.596         
    ##                                         
    ##             Sensitivity : 0.873         
    ##             Specificity : 0.739         
    ##          Pos Pred Value : 0.842         
    ##          Neg Pred Value : 0.785         
    ##              Prevalence : 0.615         
    ##          Detection Rate : 0.536         
    ##    Detection Prevalence : 0.637         
    ##       Balanced Accuracy : 0.806         
    ##                                         
    ##        'Positive' Class : 0             
    ## 

## Class based model

``` r
confusionMatrix(factor(y_hat_by_pclass), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 94 37
    ##          1 16 32
    ##                                        
    ##                Accuracy : 0.704        
    ##                  95% CI : (0.631, 0.77)
    ##     No Information Rate : 0.615        
    ##     P-Value [Acc > NIR] : 0.00788      
    ##                                        
    ##                   Kappa : 0.337        
    ##                                        
    ##  Mcnemar's Test P-Value : 0.00601      
    ##                                        
    ##             Sensitivity : 0.855        
    ##             Specificity : 0.464        
    ##          Pos Pred Value : 0.718        
    ##          Neg Pred Value : 0.667        
    ##              Prevalence : 0.615        
    ##          Detection Rate : 0.525        
    ##    Detection Prevalence : 0.732        
    ##       Balanced Accuracy : 0.659        
    ##                                        
    ##        'Positive' Class : 0            
    ## 

## Sex and Pclass combined model

``` r
confusionMatrix(factor(y_hat_sex_class), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 109  31
    ##          1   1  38
    ##                                         
    ##                Accuracy : 0.821         
    ##                  95% CI : (0.757, 0.874)
    ##     No Information Rate : 0.615         
    ##     P-Value [Acc > NIR] : 1.72e-09      
    ##                                         
    ##                   Kappa : 0.589         
    ##                                         
    ##  Mcnemar's Test P-Value : 2.95e-07      
    ##                                         
    ##             Sensitivity : 0.991         
    ##             Specificity : 0.551         
    ##          Pos Pred Value : 0.779         
    ##          Neg Pred Value : 0.974         
    ##              Prevalence : 0.615         
    ##          Detection Rate : 0.609         
    ##    Detection Prevalence : 0.782         
    ##       Balanced Accuracy : 0.771         
    ##                                         
    ##        'Positive' Class : 0             
    ## 

## F1 Score (harmonic accuracy)

``` r
#Sex based model
F_meas(factor(y_hat_Sex), test_set$Survived)
```

    ## [1] 0.857

``` r
#Class based model
F_meas(factor(y_hat_by_pclass), test_set$Survived)
```

    ## [1] 0.78

``` r
#Combined model 
F_meas(factor(y_hat_sex_class), test_set$Survived)
```

    ## [1] 0.872

# Survival by fare - LDA and QDA

``` r
#LDA
set.seed(1, sample.kind = "Rounding")
```

    ## Warning in set.seed(1, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
    ## used

``` r
train_lda <- train_set  %>% train(Survived ~ Fare, method="lda", data=.)

confusionMatrix(predict(train_lda, test_set),test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 106  51
    ##          1   4  18
    ##                                        
    ##                Accuracy : 0.693        
    ##                  95% CI : (0.62, 0.759)
    ##     No Information Rate : 0.615        
    ##     P-Value [Acc > NIR] : 0.018        
    ##                                        
    ##                   Kappa : 0.257        
    ##                                        
    ##  Mcnemar's Test P-Value : 5.55e-10     
    ##                                        
    ##             Sensitivity : 0.964        
    ##             Specificity : 0.261        
    ##          Pos Pred Value : 0.675        
    ##          Neg Pred Value : 0.818        
    ##              Prevalence : 0.615        
    ##          Detection Rate : 0.592        
    ##    Detection Prevalence : 0.877        
    ##       Balanced Accuracy : 0.612        
    ##                                        
    ##        'Positive' Class : 0            
    ## 

``` r
#QDA
set.seed(1, sample.kind = "Rounding")
```

    ## Warning in set.seed(1, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
    ## used

``` r
train_qda <- train_set  %>% train(Survived ~ Fare, method="qda", data=.)

confusionMatrix(predict(train_qda, test_set), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 106  51
    ##          1   4  18
    ##                                        
    ##                Accuracy : 0.693        
    ##                  95% CI : (0.62, 0.759)
    ##     No Information Rate : 0.615        
    ##     P-Value [Acc > NIR] : 0.018        
    ##                                        
    ##                   Kappa : 0.257        
    ##                                        
    ##  Mcnemar's Test P-Value : 5.55e-10     
    ##                                        
    ##             Sensitivity : 0.964        
    ##             Specificity : 0.261        
    ##          Pos Pred Value : 0.675        
    ##          Neg Pred Value : 0.818        
    ##              Prevalence : 0.615        
    ##          Detection Rate : 0.592        
    ##    Detection Prevalence : 0.877        
    ##       Balanced Accuracy : 0.612        
    ##                                        
    ##        'Positive' Class : 0            
    ## 

# Logistic regression models

## Age as predictor

``` r
set.seed(1, sample.kind = "Rounding")
```

    ## Warning in set.seed(1, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
    ## used

``` r
train_glm <- train_set %>% train(Survived ~ Age, method="glm", data=.)
confusionMatrix(predict(train_glm, test_set), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 110  69
    ##          1   0   0
    ##                                         
    ##                Accuracy : 0.615         
    ##                  95% CI : (0.539, 0.686)
    ##     No Information Rate : 0.615         
    ##     P-Value [Acc > NIR] : 0.533         
    ##                                         
    ##                   Kappa : 0             
    ##                                         
    ##  Mcnemar's Test P-Value : 2.7e-16       
    ##                                         
    ##             Sensitivity : 1.000         
    ##             Specificity : 0.000         
    ##          Pos Pred Value : 0.615         
    ##          Neg Pred Value :   NaN         
    ##              Prevalence : 0.615         
    ##          Detection Rate : 0.615         
    ##    Detection Prevalence : 1.000         
    ##       Balanced Accuracy : 0.500         
    ##                                         
    ##        'Positive' Class : 0             
    ## 

## Sex, class, fare and age predictors

``` r
set.seed(1, sample.kind = "Rounding")
```

    ## Warning in set.seed(1, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
    ## used

``` r
train_glm_2 <- train_set %>% train(Survived ~ Sex+Pclass+Fare+Age, method="glm", data=.)
confusionMatrix(predict(train_glm_2, test_set), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction  0  1
    ##          0 98 15
    ##          1 12 54
    ##                                         
    ##                Accuracy : 0.849         
    ##                  95% CI : (0.788, 0.898)
    ##     No Information Rate : 0.615         
    ##     P-Value [Acc > NIR] : 5.17e-12      
    ##                                         
    ##                   Kappa : 0.679         
    ##                                         
    ##  Mcnemar's Test P-Value : 0.7           
    ##                                         
    ##             Sensitivity : 0.891         
    ##             Specificity : 0.783         
    ##          Pos Pred Value : 0.867         
    ##          Neg Pred Value : 0.818         
    ##              Prevalence : 0.615         
    ##          Detection Rate : 0.547         
    ##    Detection Prevalence : 0.631         
    ##       Balanced Accuracy : 0.837         
    ##                                         
    ##        'Positive' Class : 0             
    ## 

## All predictors

``` r
set.seed(1, sample.kind = "Rounding")
train_glm_all <- train_set %>% train(Survived ~ ., method="glm", data=.)
confusionMatrix(predict(train_glm_all, test_set), test_set$Survived)
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction   0   1
    ##          0 100  17
    ##          1  10  52
    ##                                         
    ##                Accuracy : 0.849         
    ##                  95% CI : (0.788, 0.898)
    ##     No Information Rate : 0.615         
    ##     P-Value [Acc > NIR] : 5.17e-12      
    ##                                         
    ##                   Kappa : 0.675         
    ##                                         
    ##  Mcnemar's Test P-Value : 0.248         
    ##                                         
    ##             Sensitivity : 0.909         
    ##             Specificity : 0.754         
    ##          Pos Pred Value : 0.855         
    ##          Neg Pred Value : 0.839         
    ##              Prevalence : 0.615         
    ##          Detection Rate : 0.559         
    ##    Detection Prevalence : 0.654         
    ##       Balanced Accuracy : 0.831         
    ##                                         
    ##        'Positive' Class : 0             
    ##
