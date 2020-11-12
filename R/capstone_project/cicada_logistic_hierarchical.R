library("rjags")
library("dplyr")

#read file and check data structure
dat = read.table(file="Cicada.txt", header=TRUE)
head(dat)
str(dat)
dim(dat)
pairs(dat)

plot(BL ~ BW, data=dat)
plot(BL ~ WW, data=dat)

dat = na.omit(dat)


X = scale(dat[c(1:4)], center=TRUE, scale=TRUE)
head(X)
colMeans(X)
apply(X, 2, sd)


mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int +
                  b[1]*BW[i] +
                  b[2]*WL[i]+
                  b[3]*WW[i]+
                  b[4]*BL[i]



    }
    int ~ dnorm(0.0, 1.0/25)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1/1e4) # has variance 1.0
    }


} "


set.seed(92)


#data_jags = as.list(dat)
data_jags = list(y=dat$G,
                 BW=X[,"BW"],
                 WL=X[,"WL"],
                 WW=X[,"WW"],
                 BL=X[,"BL"]
                 )

head(data_jags)
params = c("int", "b")

mod1 = init_model(mod1_string, data_jags,3)
mod1_sim = simulate_model(mod1,params,5e4)
mod1_csim = combine_simu(mod1_sim)

summary(mod1_sim)
plot(mod1_sim, ask=TRUE)

pm_coef = colMeans(mod1_csim)

pm_Xb = pm_coef["int"] +  X[,c(1:4)] %*% pm_coef[1:4]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

plot(phat, jitter(data_jags$y))

(tab0.5 = table(phat>0.5, data_jags$y))

sum(diag(tab0.5)) / sum(tab0.5)


#model 2
mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int[Species[i]] +
                  b[1]*BW[i] +
                  b[2]*WL[i]+
                  b[3]*WW[i]+
                  b[4]*BL[i]



    }
    for (i in 1:max(Species)){
      int[i] ~ dnorm(mu_0, prec_0)
    }

    mu_0 ~ dnorm(0, 1/25)
    prec_0 ~ dgamma(1/2,1/2)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1/1e4) # has variance 1.0
    }


} "


set.seed(92)


#data_jags = as.list(dat)
data_jags = list(y=dat$G,
                 BW=X[,"BW"],
                 WL=X[,"WL"],
                 WW=X[,"WW"],
                 BL=X[,"BL"],
                 Species = dat$Species+1
)

head(data_jags)
params = c("int", "b")

mod2 = init_model(mod2_string, data_jags,3)
mod2_sim = simulate_model(mod2,params,5e4)
mod2_csim = combine_simu(mod2_sim)

summary(mod1_sim)
plot(mod2_sim, ask=TRUE)

pm_coef = colMeans(mod2_csim)
X = cbind(X, data_jags$Species)
head(X)
X[,X[which(X[,6] == 1)]]
X1=X[X[, 5] == 1,]
X2=X[X[, 5] == 2,]
X3=X[X[, 5] == 3,]

pm_Xb_1 = pm_coef["int[1]"] +  X1[,c(1:4)] %*% pm_coef[1:4]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

pm_Xb_2 = pm_coef["int[2]"] +  X2[,c(1:4)] %*% pm_coef[1:4]
phat_2 = 1.0 / (1.0 + exp(-pm_Xb_2))
head(phat_2)
plot(phat_2, jitter(dat$G[which(dat$Species == 1)]))

pm_Xb_3 = pm_coef["int[2]"] +  X3[,c(1:4)] %*% pm_coef[1:4]
phat_3 = 1.0 / (1.0 + exp(-pm_Xb_3))
head(phat_3)
plot(phat_3, jitter(dat$G[which(dat$Species == 2)]))

(tab0.5 = table(phat>0.5, dat$G[which(dat$Species == 0)]))
(tab0.5_2 = table(phat_2>0.5, dat$G[which(dat$Species == 1)]))
(tab0.5_3 = table(phat_3>0.5, dat$G[which(dat$Species == 2)]))

sum(diag(tab0.5_3)) / sum(tab0.5_3)
# -------------------------- helper functions  -------------------------- #

init_model = function(mod_string, data_jags, n_chains){
  mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=n_chains)
  update(mod, 1e3) #burn in
  mod
}

simulate_model =  function(mod, params, n_iter){
  coda.samples(model=mod,variable.names=params,n.iter=n_iter)

}
combine_simu = function(mod_sim){
  as.mcmc(do.call(rbind, mod_sim))
}

diagnose_covergence = function(mod_sim){
  plot(mod3_sim)

  gelman.diag(mod1_sim)
  autocorr.diag(mod3_sim)
  autocorr.plot(mod1_sim)
}

dic1 = dic.samples(mod1, n.iter=1e3)
dic2 = dic.samples(mod2, n.iter=1e3)
dic3 = dic.samples(mod2, n.iter=1e3)
