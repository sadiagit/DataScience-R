#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.
#What is the probability of guessing correctly for one question?

p <- 1/5
not_p <- 4/5

a <- 1
b <- -0.25
n<44

#Expected value point for guessing one question
avg <- a*p+b*not_p

#expected value of guessing all 44 questions
Ex <- n * (a*p + b*not_p)

#standard error of guessing all 44 questions
SE <- abs(b-a) * sqrt(p*not_p)*sqrt(n)

#Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1 - pnorm(8,Ex ,SE)

#Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")
S <-  replicate (10000, {
  X <- sample(c(1,-0.25), n,replace=TRUE, prob=c(p, not_p))
  sum(X)
})
mean(S >= 8)

#Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.

#What is the expected value of the score when guessing on this new test?
p_4 <- 1/4
p_not_4 <- 3/4
a_4 <- 1
b_4 <- 0
n <- 44
avg_4 <- a_4*p_4+b_4 *p_not_4
se_4 <- abs(b_4-a_4)*sqrt(p_4*p_not_4)*sqrt(n)

Ex_4 <- n*(a_4*p_4+b_4 *p_not_4)

#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.

#What is the lowest p such that the probability of scoring over 35 exceeds 80%?

a = 1
b = 0
n <- 44
p <- seq(0.25, 0.95, 0.05)
fp <-function(p){
   q <- 1-p
   avg <- (a*p+b*q)*n
   se <- abs(0-1)*sqrt(p*q)*sqrt(44)

   ps_g_35 <- 1 - pnorm(35, avg, se)
   ps_g_35 > 0.8
 }
sapply(p,fp)

#A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets.
#The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6.
#A gambler wants to know the chance of losing money if he places 500 bets on the roulette House Special.
#What is the expected value of the payout for one bet?
p <- 5/38
q <- 33/38
a <- 6
b <- -1
Ex <- a*p+b*q
se <- abs(b-a)*sqrt(p*q)


p <- 1-pnorm(110,100,12/sqrt(12))




