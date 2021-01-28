Reshape Data Co2
================
Sadia Boksh
19/11/2020

# Reshape Data

Change wide data to tidy data by gathering.

``` r
 co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
      setNames(1:12) %>%
    mutate(year = as.character(1959:1997))
co2_tidy <- gather(co2_wide,month,co2,-year)
```

# Plots

``` r
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()
```

![](reshape_data_co2_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

There is a seasonal effect and CO2 concentrations are highest around May
and the yearly average increased from 1959 to 1997.
