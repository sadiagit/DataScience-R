library(tidyverse)
library(ggplot2)
options(digits = 3)

# load brexit_polls object
library(dslabs)
data(brexit_polls)
head(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread


brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)
brexit_polls %>%  summarize(avg=mean(spread), s= sd(spread), avg_p=mean(x_hat), s_hat=sd(x_hat))

first_poll <- brexit_polls[1,]
#95% confidence interval of x_hat
x_hat <- first_poll$x_hat
ci <- c( x_hat-qnorm(.975)*sqrt(x_hat*(1-x_hat)/first_poll$samplesize),x_hat+qnorm(.975)*sqrt(x_hat*(1-x_hat)/first_poll$samplesize ))
ci

june_polls <- brexit_polls %>%
                    filter(enddate >= '2016-06-01') %>%
                    mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize)) %>%
                    mutate(se_d = 2*se_x_hat) %>%
                    mutate(lower= 2*x_hat -1 - 1.96*se_d, upper=2*x_hat -1 + 1.96*se_d) %>%
                    mutate(hit= d>= lower & d<= upper)

nrow(june_polls)
j_p <- june_polls %>% select(remain, leave, spread, x_hat, se_x_hat, se_d,lower,upper, hit)
z_c <- j_p %>% mutate(zero_c = 0>=lower & 0<=upper) %>% select(zero_c)
z_c
mean(z_c == TRUE)

remain_p <- j_p %>% mutate(remain_p = lower > 0 & upper > 0) %>% select(remain_p)
#remain_p
mean(remain_p == TRUE)

hit <- j_p %>% select(hit)
mean(hit == TRUE)


r<-june_polls %>% group_by(pollster) %>% summarize(n=n(), hit_rate=mean(hit)) %>% arrange(desc(hit_rate))

june_polls %>% ggplot(aes(x=poll_type, y=spread)) +
                geom_boxplot()+
                geom_point()


#Frequentist confidence intervals have the interpretation that "If you were to repeat many times the process of collecting data and computing a 95% confidence interval, then on average about 95% of those intervals would contain the true parameter value; however, once you observe data and compute an interval the true value is either in the interval or it is not, but you can't tell which." Bayesian credible intervals have the interpretation that "Your posterior probability that the parameter is in a 95% credible interval is 95%." Under what circumstances would you prefer a frequentist confidence interval, and when would you prefer a Bayesian credible interval?

combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type %>% mutate(se_hat = 2*sqrt(p_hat*(1-p_hat)/N)) %>%
                      mutate(lower = spread - 1.96*se_hat, upper = spread +1.96*se_hat)



brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

online_no <- brexit_hit%>%filter(poll_type=="Online"& hit=="FALSE") %>% nrow()
Online_yes <- brexit_hit%>%filter(poll_type=="Online"& hit=="TRUE") %>% nrow()

phone_no <- brexit_hit%>%filter(poll_type=="Telephone"& hit=="FALSE")%>% nrow()
phone_yes <- brexit_hit%>%filter(poll_type=="Telephone"& hit=="TRUE")%>% nrow()

two_by_two<- tibble(Hit=c("yes","no"), Online = c(Online_yes,Online_no), Telephone= c(phone_yes,phone_no))
rate <- totals[2,4]/(totals[2,4]+totals[1,4])


chisq_test <- two_by_two %>% select(-Hit) %>% chisq.test()

odds_online <- (Online_yes/(Online_yes+online_no))/(online_no/(online_no+Online_yes))
odds_phone <- (phone_yes/(phone_yes+phone_no))/(phone_no/(phone_no+phone_yes))
odds_online/odds_phone


brexit_polls %>% ggplot(aes(x=enddate, y=spread, col=poll_type))+
                 geom_smooth(method='loess', span=0.4)+geom_point()+geom_hline(yintercept = -0.038)

brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  mutate(vote = factor(vote))
brexit_long %>% ggplot(aes(x=enddate, y=proportion, col=vote))+geom_smooth(span=0.4, method = 'loess')


