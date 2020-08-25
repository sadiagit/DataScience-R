library(ggplot2)
library(dplyr)
set.seed(16,sample.kind = "Rounding")
act_scores <- rnorm(10000,20.9,5.7)
m <- mean(act_scores)
mu <- sd(act_scores)
l <- length(which(act_scores >= 36))
act_score_greater_than_30 <- 1-pnorm(30,m, mu)
act_score_less_than_eq_10 <- pnorm(10,m, mu)

#plot density curve
x <- seq(1,36)
f_x <- dnorm(x,20.9,5.7)
data.frame(x, f_x) %>%
ggplot(aes(x,f_x)) +
        geom_line()

# Z Scores
z_Scores <-  scale(act_scores)

#What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
x_2 <- 2*mu+m

#What is the probability of a Z-score greater than 2 (2 standard deviations above the mean)?
pr_z_greater_than_2 <- 1-pnorm(x_2, m, mu)

# A Z-score of 2 corresponds roughly to the 97.5th percentile.#
# Use qnorm() to determine the 97.5th percentile of normally distributed data with the mean and standard deviation observed in act_scores.
#What is the 97.5th percentile of act_scores?

perc_975 <- qnorm(.975,m,mu)

#Write a function that takes a value and produces the probability of an ACT score less than or equal to that value (the CDF). Apply this function to the range 1 to 36.
cdf <- function(x){
  pnorm(x, m,mu)
}
sapply(seq(1,36),cdf)

x<-qnorm(.95,20.9,5.7)


# As discussed in the Data Visualization course, we can use quantile() to determine sample quantiles from the data.
#
# Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles.
#
# In what percentile is a score of 26?

p <- seq(0.01, 0.99, 0.01)
sample_quantiles <- quantile(act_scores,p)
p[which(sample_quantiles>= 25.992660 & sample_quantiles <= 26)]

# Make a corresponding set of theoretical quantiles using qnorm() over the interval p <- seq(0.01, 0.99, 0.01) with mean 20.9 and standard deviation 5.7. Save these as theoretical_quantiles. Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
theoretical_quantiles <- qnorm(p,20.9,5.7)
data.frame(theoretical_quantiles, sample_quantiles) %>%
ggplot(aes(theoretical_quantiles, sample_quantiles))+geom_point()+geom_abline()



