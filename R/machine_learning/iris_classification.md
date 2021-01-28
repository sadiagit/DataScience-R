Iris Species Classification
================
Sadia Boksh
28/11/2020

# Explore data

First,we will remove the setosa species and we will focus on the
versicolor and virginica iris specie

``` r
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

#proportion if each species
iris %>% group_by(Species) %>% summarize(n=n()) %>% mutate(prop = n/sum(n))
```

    ## # A tibble: 2 x 3
    ##   Species        n  prop
    ##   <fct>      <int> <dbl>
    ## 1 versicolor    50   0.5
    ## 2 virginica     50   0.5

``` r
plot(iris,pch=21,bg=iris$Species)
```

![](iris_classification_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Test and train set

First let us create an even split of the data into train and test
partitions.

``` r
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
```

    ## Warning in set.seed(2, sample.kind = "Rounding"): non-uniform 'Rounding' sampler
    ## used

``` r
test_index <- createDataPartition(y, times = 1,p = 0.5,list = FALSE)
```

    ## Warning in createDataPartition(y, times = 1, p = 0.5, list = FALSE): Some
    ## classes have no records ( setosa ) and these will be ignored

``` r
test <- iris[test_index,]
train <- iris[-test_index,]
```

## Predictive analysis

Next we will figure out the singular feature in the dataset that yields
the greatest overall accuracy when predicting species

### Sepal Length accuracy

``` r
range_sl <- range(iris$Sepal.Length)
cut_off <- seq(range_sl[1],range_sl[2], by=0.1)
accuracy <-  map_dbl(cut_off,function(x){
  
y_hat <- ifelse(train$Sepal.Length > x,  "virginica","versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

df <- data.frame(Sepal_Length=cut_off,accuracy=accuracy)
df %>% ggplot(aes(Sepal_Length,accuracy))+geom_point()+geom_line()
```

![](iris_classification_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

### Sepal Width accuracy

``` r
range_sw <- range(iris$Sepal.Width)
cut_off <- seq(range_sw[1],range_sw[2], by=0.1)
accuracy <-  map_dbl(cut_off,function(x){
  
y_hat <- ifelse(train$Sepal.Width > x,  "virginica","versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

df <- data.frame(Sepal_Width=cut_off,accuracy=accuracy)
df %>% ggplot(aes(Sepal_Width,accuracy))+geom_point()+geom_line()
```

![](iris_classification_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

### Petal Width accuracy

``` r
range_pw <- range(iris$Petal.Width)
cut_off <- seq(range_pw[1],range_pw[2], by=0.1)
accuracy <-  map_dbl(cut_off,function(x){
  
  y_hat <- ifelse(train$Petal.Width > x, "virginica","versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

df <- data.frame(Petal_Width=cut_off,accuracy=accuracy)
df %>% ggplot(aes(Petal_Width,accuracy))+geom_point()+geom_line()
```

![](iris_classification_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Petal Length accuracy

``` r
range_pl <- range(iris$Petal.Length)
cut_off <- seq(range_pl[1],range_pl[2], by=0.1)
accuracy <-  map_dbl(cut_off,function(x){
  
y_hat <- ifelse(train$Petal.Length > x, "virginica","versicolor" ) %>% factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

df <- data.frame(Petal_Length=cut_off,accuracy=accuracy)
df %>% ggplot(aes(Petal_Length,accuracy))+geom_point()+geom_line()
```

![](iris_classification_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

In the train set, it is petal length yields best overall accuracy.

Now we will use Petal Length cutoff to find the overall accuracy of test
data.

``` r
best_cutoff <- df[which.max(accuracy),"Petal_Length"]
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica","versicolor" ) %>% factor(levels = levels(test$Species))
mean(y_hat == test$Species)
```

    ## [1] 0.9

The overall accuracy was lower in the test data. This can happen often
if we overtrain.

Given that we know the test data, we can treat it like we did our
training data to see if the same feature with a different cutoff will
optimize our predictions.

``` r
acuracy_checker <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    sapply(rangedValues,function(i){
        y_hat <- ifelse(x>i,'virginica','versicolor')
        mean(y_hat==test$Species)
    })
}
predictions <- apply(test[,-5],2,acuracy_checker)
sapply(predictions,max) 
```

    ## Sepal.Length  Sepal.Width Petal.Length  Petal.Width 
    ##         0.78         0.64         0.90         0.94

In the test set, it is petal width that optimizes the overall accuracy.

Now we will use both petal length and width as predictors.

First, we will find the best cutoff for both petal length and petal
width using train data set

``` r
max_acuracy <- function(x){
    rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
    accuracy <- sapply(rangedValues,function(i){
        y_hat <- ifelse(x>i,'virginica','versicolor')
        mean(y_hat==train$Species)
    })
    rangedValues[which.max(accuracy)]
}
#best cutoff for petal length and width using train data set
cutoffs <- apply(train[,c(3:4)],2,max_acuracy)

#using the best cutoff of each predictors predict the species of test dataset
y_hat <- ifelse(test$Petal.Length >cutoffs[1] | test$Petal.Width >cutoffs[2],'virginica','versicolor')
mean(y_hat==test$Species)
```

    ## [1] 0.88

Overall accuracy is 0.88 when using both petal length and petal width as
predictors. This is because the high correlation between petal length
and petal width.

``` r
train %>% summarise(cor(Petal.Length, Petal.Width))
```

    ##   cor(Petal.Length, Petal.Width)
    ## 1                      0.8339005
