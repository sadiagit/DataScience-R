library("MASS")
library("rjags")
data("OME")
?OME # background on the data
head(OME)

any(is.na(OME)) # check for missing values
dat = subset(OME, OME != "N/A") # manually remove OME missing values identified with "N/A"
dat$OME = factor(dat$OME)
str(dat)

plot(dat$Age, dat$Correct / dat$Trials )
plot(dat$OME, dat$Correct / dat$Trials )
plot(dat$Loud, dat$Correct / dat$Trials )
plot(dat$Noise, dat$Correct / dat$Trials )

library("corrplot")
Cor = cor(dat)
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black",
         add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")

mod_glm = glm(Correct/Trials ~ Age + OME + Loud + Noise, data=dat, weights=Trials, family="binomial")
summary(mod_glm)

plot(residuals(mod_glm, type="deviance"))
plot(fitted(mod_glm), dat$Correct/dat$Trials)


X = model.matrix(mod_glm)[,-1] # -1 removes the column of 1s for the intercept
head(X)

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

data_jags = as.list(as.data.frame(X))
data_jags$y = dat$Correct # this will not work if there are missing values in dat (because they would be ignored by model.matrix). Always make sure that the data are accurately pre-processed for JAGS.
data_jags$n = dat$Trials
str(data_jags) # make sure that all variables have the same number of observations (712).
params = c("b0", "b")

mod_ome = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod_ome, 1e3)

mod_ome_sim = coda.samples(model=mod_ome,
                        variable.names=params,
                        n.iter=5e3)
mod_ome_csim = as.mcmc(do.call(rbind, mod_ome_sim))
raftery.diag(mod_ome_sim)
gelman.diag(mod_ome_sim)
autocorr.diag(mod_ome_sim)
autocorr.plot(mod_ome_sim)
effectiveSize(mod_ome_sim)
summary(mod_ome_sim)
predictor =  rbind(c(60,0,50,0))
pm_coef = colMeans(mod_ome_csim)
pm_Xb = pm_coef["b0"] + predictor %*% pm_coef[1:4]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

pm_Xb_all = pm_coef["b0"] + X[,1:4] %*% pm_coef[1:4]
phat_all = 1.0 / (1.0 + exp(-pm_Xb_all))
head(phat_all)

tab0.7 = table(phat_all > 0.7, (dat$Correct / dat$Trials) > 0.7)
sum(diag(tab0.7)) / sum(tab0.7)

dic1 = dic.samples(mod_ome, n.iter=1e3)

