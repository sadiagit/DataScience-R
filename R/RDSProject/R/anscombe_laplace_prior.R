library("car")
data("Anscombe")
head(Anscombe)
?Anscombe

Xc = scale(Anscombe, center=TRUE, scale=TRUE)
str(Xc)

library("rjags")

mod_string = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b[1]*income[i] + b[2]*young[i] + b[3]*urban[i]
    }

    for (i in 1:3) {
        b[i] ~ ddexp(0.0, 1)
    }

    prec ~ dgamma(1.0/2.0, 1.0*1.0/2.0)
    	## Initial guess of variance based on overall
    	## variance of education variable. Uses low prior
    	## effective sample size. Technically, this is not
    	## a true 'prior', but it is not very informative.
    sig2 = 1.0 / prec
    sig = sqrt(sig2)
} "
set.seed(72)

colMeans(Xc)
apply(Xc, 2, sd)
data_jags = as.list(data.frame(Xc))
params_l = c("b" ,"sig")

inits_l = function() {
  inits = list("b"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod_jags_laplace = jags.model(textConnection(mod_string), data=data_jags, inits=inits_l, n.chains=3)
update(mod_jags_laplace, 1000) # burn-in

mod_jags_lap_sim = coda.samples(model=mod_jags_laplace,
                            variable.names=params_l,
                            n.iter=1e4)

mod_jags_lap_csim = do.call(rbind, mod_jags_lap_sim) # combine multiple chains

plot(mod_jags_lap_sim)
summary(mod_jags_lap_sim)
