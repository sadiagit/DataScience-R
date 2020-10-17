#theta = success rate, m=10000 samples,
m=10000
#posterior distb of theta ~ beta(5,3), with mean 5/8
a = 5
b = 3
theta = rbeta(n=m, shape1 = a, shape2 = b)

#Use R to simulate a large number of samples (more than 10,000) from the posterior distribution for theta
#and use these samples to approximate the posterior mean for Laura's odds of success E(theta/1-theta)
#).
mean(theta/(1-theta))

#prob (theta/(1-theta)) > 1
mean(theta/(1-theta)>1)

# approx the value of 0.3 quantile of N(0,1).(p(Z <  z) = .3 )
theta_std_norm = rnorm(n=1e5)
quantile(x=theta_std_norm, probs = 0.3)

#theoretical
qnorm(0.3)



