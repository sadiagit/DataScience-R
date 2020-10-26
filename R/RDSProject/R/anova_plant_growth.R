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

mod_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dnorm(mu[grp[i]], prec)
    }

    for (j in 1:3) {
        mu[j] ~ dnorm(0.0, 1.0/1.0e6)
    }

    prec ~ dgamma(5/2.0, 5*1.0/2.0)
    sig = sqrt( 1.0 / prec )
} "
set.seed(82)
str(PlantGrowth)
data_jags = list(y=PlantGrowth$weight,
                 grp=as.numeric(PlantGrowth$group))

#PlantGrowth[PlantGrowth$group == 'ctrl',]

params = c("mu", "sig")

inits = function() {
  inits = list("mu"=rnorm(3,0.0,100.0), "prec"=rgamma(1,1.0,1.0))
}

mod = jags.model(textConnection(mod_string), data=data_jags, inits=inits, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains
plot(mod_sim)

gelman.diag(mod_sim)
autocorr.diag(mod_sim)
effectiveSize(mod_sim)

pm_params = colMeans(mod_csim)

yhat = pm_params[1:3][data_jags$grp]
#resid = data_jags$y - yhat
#plot(resid)
mod_csim = as.mcmc(do.call(rbind, mod_sim)) # combined chains

#check model
summary(mod_csim)
summary(mod_var_csim)

#DIC check
dic1 = dic.samples(mod, n.iter = 1e5)
dic2 = dic.samples(mod_var, n.iter = 1e5)
dic1-dic2 # mod_var is preferred as DIC smaller

#HPD interval - higher posterior interval shortest interval
HPDinterval(mod_csim[,3] - mod_csim[,1])

head(mod_csim)
mod_csim[,3]


mod_cm = lm(weight ~ -1 + group, data=PlantGrowth)
summary(mod_cm)

