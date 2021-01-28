Predicting Cicada Body Length using Bayesian Data Analysis
================
Sadia Boksh
13/11/2020

### Data

Lets explore cicada data.

``` r
head(cicada_df)
```

    ##     BW WL WW BL G Species
    ## 1 0.25 28 11 28 0       0
    ## 2 0.16 26 11 22 1       0
    ## 3 0.26 31 11 27 0       2
    ## 4 0.16 26  9 21 1       0
    ## 5 0.26 30 12 26 0       0
    ## 6 0.25 27 11 25 0       0

Structure of the data:

``` r
str(cicada_df)
```

    ## 'data.frame':    104 obs. of  6 variables:
    ##  $ BW     : num  0.25 0.16 0.26 0.16 0.26 0.25 0.15 0.15 0.1 0.22 ...
    ##  $ WL     : int  28 26 31 26 30 27 28 29 24 27 ...
    ##  $ WW     : int  11 11 11 9 12 11 11 10 9 11 ...
    ##  $ BL     : int  28 22 27 21 26 25 26 25 22 25 ...
    ##  $ G      : int  0 1 0 1 0 0 1 1 1 0 ...
    ##  $ Species: int  0 0 2 0 0 0 0 0 0 0 ...

``` r
cicada_df %>% ggplot(aes(x=BW,y=BL, group=Species, col=Species))+
               geom_point()
```

![](cicada_bl_pred_files/figure-gfm/Plots-1.png)<!-- -->

``` r
cicada_df %>% ggplot(aes(x=WL,y=BL, group=Species, col=Species))+
               geom_point()
```

![](cicada_bl_pred_files/figure-gfm/Plots-2.png)<!-- -->

``` r
cicada_df %>% ggplot(aes(x=WW,y=BL, group=Species, col=Species))+
               geom_point()
```

![](cicada_bl_pred_files/figure-gfm/Plots-3.png)<!-- -->

``` r
cicada_df %>% ggplot(aes(x=WW,y=WL, group=Species, col=Species))+
               geom_point()
```

![](cicada_bl_pred_files/figure-gfm/Plots-4.png)<!-- -->
