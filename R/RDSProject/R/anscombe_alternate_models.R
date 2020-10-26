#Alternative models

library("rjags")

init_jags_model = function(b_n, model_string){
  data_jags = as.list(Anscombe)


  inits_fn = function() {
    inits = list("b0"=rnorm(1,0.0,100.0) ,"b"=rnorm(b_n,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
  }

  jags.model(textConnection(model_string), data=data_jags, inits=inits_fn, n.chains=3)
}
run_sampler = function(mod_jags,burn_in){

  params = c("b0","b" ,"sig")
  update(mod_jags, burn_in) # burn-in

  coda.samples(model=mod_jags,variable.names=params,n.iter=1e4)


}

#Alternate model1

mod_string_alt = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]
    }

    b0 ~ dnorm(0.0, 1.0/1.0e6)
    for (i in 1:2) {
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
mod_jags_alt1 = init_jags_model(2, mod_string_alt)
mod_jags_alt1_sim = run_sampler(mod_jags_alt1, 1000)
mod_jags_alt1_csim = do.call(rbind, mod_jags_alt1_sim)

#model check
plot(mod_jags_alt1_sim)
summary(mod_jags_alt1_sim)

# Alternate model2 -added a transformed covariate

mod_string_alt2 = " model {
    for (i in 1:length(education)) {
        education[i] ~ dnorm(mu[i], prec)
        mu[i] = b0 + b[1]*income[i] + b[2]*young[i]  + b[3]*young[i]*income[i]
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
mod_jags_alt2 = init_jags_model(3, mod_string_alt2)
mod_jags_alt2_sim = run_sampler(mod_jags_alt2, 1000)
mod_jags_alt2_csim = do.call(rbind, mod_jags_alt2_sim)

#model check
plot(mod_jags_alt2_sim)
summary(mod_jags_alt2_sim)

#DIC check (Deviance Information Criteria)
dic.samples(mod_jags_alt2, 1e5)
dic.samples(mod_jags_alt1, 1e5)
dic.samples(mod_jags, 1e5)

#looks like our original model has better DIC (penalized deviance)
summary(mod_jags_sim)

#posterior prob that coefficient of income will be > 0
income_coeff_positive = mod_jags_csim[,1] > 0
mean(income_coeff_positive)


