
theta = seq(from=0,to=100, by=0.1)
#plot(theta, dbeta(theta,1,5), type='l')


plot(theta,dexp(theta,10/390),type='l')

post_mean <- 1/6
pbeta(0.5,1,5)
qbeta(.975,8,16)
pbeta(.35,8,16)
pbeta(.35,8,24)
ppois(0.05,11.17)
qgamma(0.975,10,390)
pgamma(0.1,6,93.5)
qnorm(0.975,96.17,sqrt(0.042))

pnorm()
pnorm(100,96.16666,sqrt(0.0416666))

z <- rgamma(n=1000, shape=3, rate=200)
x <- 1/z
mean(x)

z <- rgamma(1000, shape=16.5, rate=6022.9)
sig2 <- 1/z
mu_b <- rnorm(1000, mean=609.3, sd=sqrt(sig2/27.1))


z <- rgamma(1000, shape=18, rate=6796.44)
sig2 <- 1/z
mu_a <- rnorm(1000, mean=622.4, sd=sqrt(sig2/30.1))
quantile(x=mu, probs = c(0.025,0.975))

#improper prior
#plot for Jeffreys prior for a Bernoulli/binomial success probability pp?

#Hint: The Jeffreys prior in this case is Beta(1/2, 1/2).

theta <- seq(from=0,to=1,by=0.1)
plot(theta, dbeta(theta,.5,.5), type="l")
