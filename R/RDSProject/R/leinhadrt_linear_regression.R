install.packages("car")
library("car")
data("Leinhardt")
?Leinhardt

#Explore data
head(Leinhardt)

#inspect the structure of the df
str(Leinhardt)

#explore correlation
pairs(Leinhardt)

#We’ll start with a simple linear regression model that relates infant mortality to per capita income.

#First plot correlation between income and infant mortality
plot(infant ~ income, data=Leinhardt) #PLot reveals non linear relationship

#let's check the distributions
hist(Leinhardt$infant) #distribution is positive right-skewed
hist(Leinhardt$income)

#Since infant mortality and per capita income are positive and right-skewed quantities, we consider modeling them on the logarithmic scale.
#A linear model appears much more appropriate on this scale.
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4120293/
Leinhardt$loginfant = log(Leinhardt$infant)
Leinhardt$logincome = log(Leinhardt$income)

#Modeling
#The reference Bayesian analysis (with a noninformative prior) is available directly in R.
lmod = lm(loginfant ~ logincome, data=Leinhardt)
summary(lmod)

plot(loginfant ~ logincome, data=Leinhardt) # it looks like we can fit a linear model on log transformed data

#Model in JAGS
dat = na.omit(Leinhardt)
library("rjags")


mod1_string = " model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i]
    }

    for (i in 1:2) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }

    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "

set.seed(72)
data1_jags = list(y=dat$loginfant, n=nrow(dat),
                  log_income=dat$logincome)

params1 = c("b", "sig")

inits1 = function() {
  inits = list("b"=rnorm(2,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod1 = jags.model(textConnection(mod1_string), data=data1_jags, inits=inits1, n.chains=3)
update(mod1, 1000) # burn-in

mod1_sim = coda.samples(model=mod1,
                        variable.names=params1,
                        n.iter=5000)

mod1_csim = do.call(rbind, mod1_sim) # combine multiple chains

plot(mod1_sim)
gelman.diag(mod1_sim)
autocorr.diag(mod1_sim)
autocorr.plot(mod1_sim)
effectiveSize(mod1_sim)
summary(mod1_sim)
summary(lmod)

lmod0 = lm(infant ~ income, data=Leinhardt)
plot(resid(lmod0)) # to check independence (looks okay)
plot(predict(lmod0), resid(lmod0)) # to check for linearity, constant variance (looks bad)

qqnorm(resid(lmod0)) # to check Normality assumption (we want this to be a straight line)


X = cbind(rep(1.0, data1_jags$n), data1_jags$log_income)
(pm_params1 = colMeans(mod1_csim)) # posterior mean
yhat1 = drop(X %*% pm_params1[1:2]) # drop converts matrix to vector
resid1 = data1_jags$y - yhat1 # res = obs - pred
plot(resid1) # against data index
plot(yhat1, resid1) # against predicted values


#Additional Covariates - adding oil indicator as covariate

library("rjags")

mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }

    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }

    prec ~ dgamma(5/2.0, 5*10.0/2.0)
    sig = sqrt( 1.0 / prec )
} "


set.seed(73)
data2_jags = list(y=dat$loginfant, log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))
data2_jags$is_oil

params2 = c("b", "sig")

inits2 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod2 = jags.model(textConnection(mod2_string), data=data2_jags, inits=inits2, n.chains=3)
update(mod2, 1e3) # burn-in

mod2_sim = coda.samples(model=mod2,
                        variable.names=params2,
                        n.iter=5e3)

mod2_csim = as.mcmc(do.call(rbind, mod2_sim)) # combine multiple chains


#convergence check
plot(mod2_sim)

#scale reduction factor
gelman.diag(mod2_sim)

#autocorrelation

autocorr.diag(mod2_csim)

#plot autocorr
autocorr.plot(mod2_sim)

#effective sample size number of independent sample requires to once reached the stationary state
#The chain from post0 has 10,000 iterations, but an effective sample size of about 2,500. That is, this chain essentially provides the equivalent of 2,500 independent Monte Carlo samples.
#Autocorrelation is a major component in calculating the Monte Carlo effective sample size of your chain.
#The Monte Carlo effective sample size is how many independent samples from the stationary distribution you would have to draw to have equivalent information in your Markov chain.

summary(mod2_sim)

#Residuals check

X2 = cbind(rep(1.0, data1_jags$n), data2_jags$log_income, data2_jags$is_oil)
head(X2)
pm_params2 =  colMeans(mod2_csim)
yhat2 =  drop(X2 %*% pm_params2[1:3])
resid2 =  data2_jags$y - yhat2
plot(resid2) #independence check

plot(yhat2, resid2) #linearity, error variance check

#t likelihood to handle outliers

mod3_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dt( mu[i], tau, df )
        mu[i] = b[1] + b[2]*log_income[i] + b[3]*is_oil[i]
    }

    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }

    df = nu + 2.0 # we want degrees of freedom > 2 to guarantee existence of mean and variance
    nu ~ dexp(1.0)

    tau ~ dgamma(5/2.0, 5*10.0/2.0) # tau is close to, but not equal to the precision
    sig = sqrt( 1.0 / tau * df / (df - 2.0) ) # standard deviation of errors
} "

set.seed(73)
data3_jags = list(y=dat$loginfant, log_income=dat$logincome,
                  is_oil=as.numeric(dat$oil=="yes"))
data3_jags$is_oil

params3 = c("b", "sig")

inits3 = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "tau"=rgamma(1,1.0,1.0))
}

mod3 = jags.model(textConnection(mod3_string), data=data3_jags, inits=inits3, n.chains=3)
update(mod3, 1e3) # burn-in

mod3_sim = coda.samples(model=mod3,
                        variable.names=params3,
                        n.iter=5e3)

mod3_csim = as.mcmc(do.call(rbind, mod3_sim)) # combine multiple chains


#convergence check
plot(mod3_csim)

#scale reduction factor
gelman.diag(mod3_sim)

#autocorrelation

autocorr.diag(mod3_csim)

#plot autocorr
autocorr.plot(mod3_sim)

#effective sample size number of independent sample requires to once reached the stationary state
#The chain from post0 has 10,000 iterations, but an effective sample size of about 2,500. That is, this chain essentially provides the equivalent of 2,500 independent Monte Carlo samples.
#Autocorrelation is a major component in calculating the Monte Carlo effective sample size of your chain.
#The Monte Carlo effective sample size is how many independent samples from the stationary distribution you would have to draw to have equivalent information in your Markov chain.

#summary(mod2_sim)

#Residuals check

X3 = cbind(rep(1.0, data1_jags$n), data2_jags$log_income, data2_jags$is_oil)
head(X3)
pm_params3 =  colMeans(mod3_csim)
yhat3 =  drop(X3 %*% pm_params3[1:3])
resid3 =  data2_jags$y - yhat3
plot(resid3) #independence check

plot(yhat3, resid3) #linearity, error variance check

dic.samples(mod1, n.iter=1e3)
dic.samples(mod2, n.iter=1e3)

dic.samples(mod3, n.iter=1e3)




