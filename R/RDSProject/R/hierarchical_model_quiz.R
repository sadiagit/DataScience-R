dat = read.csv(file="C:/Dev/DataScience/R_doc/Bayes/pctgrowth.csv", header=TRUE)
head(dat)
mod_string = "model{
  for (i in 1:length(y)) {
    y[i] ~ dnorm(mu[i], prec)
    mu[i] = theta[grp[i]]
  }

  for (j in 1:max(grp)) {
    theta[j] ~ dnorm(theta0, prec_a)
  }

  theta0 ~ dnorm(0.0, 1.0/1.0e6)
  prec_a ~ dgamma(1/2.0, 1*3/2.0)
  tau = sqrt( 1.0 / prec_a )


  prec ~ dgamma(1, 1)
  sig = sqrt( 1.0 / prec )
}"

set.seed(113)

data_jags = as.list(dat)

params = c("theta", "sig", "tau")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)
summary(mod_sim)

means_anova = tapply(dat$y, INDEX=dat$grp, FUN=mean)

plot(means_anova)
points(theta, col="red") ## where means_theta are the posterior point estimates for the industry means.
