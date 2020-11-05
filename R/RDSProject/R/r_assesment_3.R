library(dslabs)
library(dplyr)

data(heights)
options(digits = 3)

head(heights)
avg_h <- mean(heights$height)
ind <- heights$height > avg_h & heights$sex == 'Female'
h1 <- heights$height[ind]
sum(ind)

mean(heights$sex == 'Female')
heights$height[which.min(heights$height)]
match(50, heights$height)

heights$sex[1032]

rnorm(1000, 0, 1/25.0)

data("esoph")
head(esoph)

esoph %>% filter(agegp == '25-34' & ncases > 0)
esoph[,esoph$agegp == '25-34' & esoph$ncases > 0]
heights$height[which.max(heights$height)]

x<-50:82

sum(!x %in% heights$height)

avg_ht_cm <- heights %>% mutate(ht_cm=height*2.54) %>% summarise(avg = mean(ht_cm))
heights[18, "ht_cm"]

females <- heights %>% filter(sex == 'Female')
dim(females)
mean(females$ht_cm)
library(dslabs)
data(olive)
head(olive)
?olive

plot(olive$palmitoleic, olive$stearic)

pairs(olive[,3:9])
hist(olive$eicosenoic)
boxplot(palmitic ~ region,data=olive)

data("CO2")
head(CO2)
CO2
