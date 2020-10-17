Q = matrix(c(0,1,
             0.3, 0.7),nrow=2, byrow=TRUE)

Qi = matrix(c(1,0
              ),nrow=1, byrow=TRUE)
q4 = Q
for (i in 2:3) {
  Q4 = Q4 %*% Q
}
round(Q4, 3)

Qi %*% Q %*% Q %*% Q


a <- 2
b <- -1
c <- -4

x<- (-b+sqrt(b^2-4*a*c))/2*a


log(1024,4)
help(log)


install.packages("dslabs")
library(dslabs)
data(movielens)
str(movielens)


class(movielens$genres)

nlevels(movielens$genres)

help(runif)
runif(1)


x <- c(2, 43, 27, 96, 18)
max(x)



name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time <- time / 60
spped<- distance / time
