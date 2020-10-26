library("car")  # load the 'car' package
data("Anscombe")  # load the data set
?Anscombe  # read a description of the data
head(Anscombe)  # look at the first few lines of the data
pairs(Anscombe)

hist(Anscombe$education) #distribution is positive right-skewed
hist(Anscombe$income)

#Fit a reference (noninformative) Bayesian linear model to the Anscombe data with education expenditures as the response variable and include all three other variables as predictors. Use the \tt lmlm function in R.
mod_ref = lm(education ~ income+young+urban, data=Anscombe)
summary(mod_ref)

#jags model

library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }

    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:3) {
        b[i] ~ dnorm(0.0, 1.0/1.0e6)
    }

    prec ~ dgamma(1.0/2.0, 1.0*1500.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
set.seed(72)
data_jags = as.list(Anscombe)
params1 = c("b0","b" ,"sig")

inits1 = function() {
  inits = list("b0"=rnorm(1,0.0,100.0) ,"b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod_jags = jags.model(textConnection(mod_string), data=data_jags, inits=inits1, n.chains=3)
update(mod_jags, 1000) # burn-in

mod_jags_sim = coda.samples(model=mod_jags,
                        variable.names=params1,
                        n.iter=1e4)

mod_jags_csim = do.call(rbind, mod_jags_sim) # combine multiple chains

plot(mod_jags_sim)

gelman.diag(mod_jags_sim)
autocorr.diag(mod_jags_sim)
autocorr.plot(mod_jags_sim)
effectiveSize(mod_jags_sim)
summary(mod_jags_sim)
summary(mod_ref)

plot(predict(mod_ref), resid(mod_ref)) #linearity check
qqnorm(resid(mod_ref)) #normality check


mean(mod_jags_csim[,1] > 0)
