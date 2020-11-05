library("MASS")
data("OME")

dat = subset(OME, OME != "N/A")
dat$OME = factor(dat$OME) # relabel OME
dat$ID = as.numeric(factor(dat$ID)) # relabel ID so there are no gaps in numbers (they now go from 1 to 63)

## Original reference model and covariate matrix
mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
X = model.matrix(mod_glm)[,-1]

## Original model (that needs to be extended)
mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = b0 + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}

	b0 ~ dnorm(0.0, 1.0/5.0^2)
	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}

} "



#hierarchical model on id

h_mod_string = " model {
	for (i in 1:length(y)) {
		y[i] ~ dbin(phi[i], n[i])
		logit(phi[i]) = a[ID[i]] + b[1]*Age[i] + b[2]*OMElow[i] + b[3]*Loud[i] + b[4]*Noiseincoherent[i]
	}

  for (i in 1:max(ID)){
    a[i] ~ dnorm(mu_a, prec_a)
  }

  mu_a ~ dnorm(0,1/10^2)
  prec_a ~ dgamma(1/2,1/2)

  tau = sqrt(1/prec_a)

	for (j in 1:4) {
		b[j] ~ dnorm(0.0, 1.0/4.0^2)
	}

} "
data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct
data_jags$n = dat$Trials
data_jags$ID = dat$ID

params = c("a", "b", "tau")

h_mod_ome = jags.model(textConnection(h_mod_string), data=data_jags, n.chains=3)
update(h_mod_ome, 1e3)

h_mod_ome_sim = coda.samples(model=h_mod_ome,
                           variable.names=params,
                           n.iter=5e3)
h_mod_ome_csim = as.mcmc(do.call(rbind, h_mod_ome_sim))
raftery.diag(h_mod_ome_sim)
gelman.diag(h_mod_ome_sim)
autocorr.diag(h_mod_ome_sim)
autocorr.plot(h_mod_ome_sim)
effectiveSize
plot(h_mod_ome_sim)
dic2 = dic.samples(h_mod_ome, n.iter=1e3)

summary(h_mod_ome_sim)
