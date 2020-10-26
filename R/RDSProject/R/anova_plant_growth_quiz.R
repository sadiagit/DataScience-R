data("PlantGrowth")
?PlantGrowth
head(PlantGrowth)

#boxplots to visualize data rather than scatter plot as categorical
boxplot(weight ~ group, data=PlantGrowth)

lmod = lm(weight ~ group, data=PlantGrowth)
summary(lmod)
anova(lmod)
model.matrix(lmod)
library("rjags")

#separate variance for each group
mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec[grp[i]])
    }

    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
        prec[j] ~ dgamma(5/2.0, 5*1.0/2.0)
    }

    sig = sqrt( 1.0 / prec )
} "
set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight,
                 grp=as.numeric(PlantGrowth$group))

#PlantGrowth[PlantGrowth$group == 'ctrl',]

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(3,1.0,1.0))
}

mod_var = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod_var, 1e3)

mod_var_sim = coda.samples(model=mod_var,
                       variable.names=params,
                       n.iter=5e3)
mod_var_csim = as.mcmc(do.call(rbind, mod_var_sim)) # combined chains
plot(mod_var_sim)
summary(mod_var_csim)

gelman.diag(mod_var_sim)
autocorr.diag(mod_var_sim)
effectiveSize(mod_var_sim)

pm_params = colMeans(mod_var_sim)

yhat = pm_params[1:3][data_jags$grp]
#resid = data_jags$y - yhat
#plot(resid)

