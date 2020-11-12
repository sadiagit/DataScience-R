library("rjags")
library("dplyr")

#read file and check data structure
dat = read.table(file="Cicada.txt", header=TRUE)
head(dat)
str(dat)
dim(dat)
pairs(dat[,1:4])
#Explore data
plot(BL ~ BW+WW+WL, data=dat)
abline()
plot(BL ~ WW, data=dat)
boxplot(BL ~ G, data=dat)
boxplot(BL ~ Species, data=dat)
hist(dat$BL)

#Wrangle data
any(is.na(dat))
dat = na.omit(dat)

dat$Species = dat$Species+1
dat$G = dat$G+1
data_jags = as.list(dat)

#---------------------------- simple model with 4 variables ---------------#
s4_mod_string = " model {
    for (i in 1:length(BL)) {
        BL[i] ~ dnorm(mu[i], prec)
        mu[i] = int +
                  b[1]*BW[i] +
                  b[2]*WL[i]+
                  b[3]*WW[i]+
                  b[4]*G[i]

    }
    int ~ dnorm(0.0, 1.0/1e4)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1/1e4)
    }

    prec ~ dgamma(1/2.0, 1*2/2.0)
    sig = sqrt(1 / prec)
} "


set.seed(92)

params = c("int", "b")

s4_mod = init_model(s4_mod_string, data_jags,3)
s4_mod_sim = simulate_model(s4_mod,params,5e4)
s4_mod_csim = combine_simu(s4_mod_sim)
s4_dic = dic.samples(s4_mod, n.iter=1e3)

# --------------------- Hierarchical model -------------------- #
h_mod_string = " model {
    for (i in 1:length(BL)) {
        BL[i] ~ dnorm(mu[i], prec)
        mu[i] = a[Species[i]] +
                  b[1]*BW[i] +
                  b[2]*WL[i]+
                  b[3]*WW[i]+
                  b[4]*G[i]

    }
    for (i in 1:max(Species)) {
       a[i] ~ dnorm(a0, prec_a)
    }

    a0 ~ dnorm(0.0, 1.0/1.0e4)
    prec_a ~ dgamma(1/2.0, 1*2.0/2.0)
    tau = sqrt( 1.0 / prec_a )

    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1/1e4)
    }

    prec ~ dgamma(1/2.0, 1*2/2.0)
    sig = sqrt(1 / prec)
} "


set.seed(92)

params = c("a0","a", "b", "sig", "tau")

h_mod = init_model(h_mod_string, data_jags,3)
h_mod_sim = simulate_model(h_mod,params,1e6)
h_mod_csim = combine_simu(h_mod_sim)
(h_dic = dic.samples(h_mod, n.iter=1e3))


# ---------------------- hierarchical model -------------------#
h_modx_string = " model {
    for (i in 1:length(BL)) {
        BL[i] ~ dnorm(mu[i], prec)
        mu[i] = a[Species[i]] +
                  b[1]*BW[i] +
                  b[2]*WL[i]+
                  b[3]*WW[i]+
                  b[4]*G[i] +
                  b_intx * WL[i] * G[i]


    }
    for (i in 1:max(Species)) {
       a[i] ~ dnorm(a0, prec_a)
    }

    a0 ~ dnorm(0.0, 1.0/1.0e4)
    prec_a ~ dgamma(1/2.0, 1*2.0/2.0)
    tau = sqrt( 1.0 / prec_a )

    b_intx ~ dnorm(0.0, 1/1e4)
    for (j in 1:4) {
        b[j] ~ dnorm(0.0, 1/1e4)
    }

    prec ~ dgamma(1/2.0, 1*2/2.0)
    sig = sqrt(1 / prec)
} "


set.seed(92)

params = c("a0","a", "b","b_intx", "sig", "tau")

h_modx = init_model(h_modx_string, data_jags,3)
h_modx_sim = simulate_model(h_modx,params,1e6)
h_modx_csim = combine_simu(h_modx_sim)

# ------------------ Model Checking --------------------#
(hx_dic = dic.samples(h_modx, n.iter=1e3))

summary(h_modx_sim)

#---------------- Residual Checks ---------------------#
#prediction
pm_coef = colMeans(h_modx_csim)# posterior mean


X = cbind(data_jags$BW,data_jags$WL,data_jags$WW,data_jags$G, data_jags$Species, data_jags$WL*data_jags$G)
head(X)

X1=X[X[, 5] == 1,]
X2=X[X[, 5] == 2,]
X3=X[X[, 5] == 3,]

yhat1 = drop(pm_coef["a[1]"]+ X1[,c(1:4,6)] %*% pm_coef[c(5:9)])
resid1 = dat$BL[which(dat$Species == 1)] - yhat1
plot(resid1,xlab="index", ylab="resid1", main="Residual check for species 1 (tredecula)") # against data index
plot(yhat1, resid1,xlab="yhat1", ylab="resid1", main="Residuals vs preditions for species 1 (tredecula)")


qqnorm(resid1)

yhat2 = drop(pm_coef["a[2]"]+ X2[,c(1:4,6)] %*% pm_coef[c(5:9)])
resid2 = dat$BL[which(dat$Species == 2)] - yhat2
plot(resid2) # against data index
plot(yhat2, resid2)

yhat3 = drop(pm_coef["a[3]"]+ X3[,c(1:4,6)] %*% pm_coef[c(5:9)])
resid3 = dat$BL[which(dat$Species == 3)] - yhat3
plot(resid3) # against data index
plot(yhat3, resid3)

#new predictions
cicada = c(.20,24,8,2,48)
(n_sim = nrow(h_modx_csim))
mu_pred1 =  h_modx_csim[,"a[1]"] + h_modx_csim[,c(5:9)] %*% cicada
y_pred_1 =  rnorm(n=n_sim, mu_pred1, h_modx_csim[, "sig"])
mu_pred2 =  h_modx_csim[,"a[2]"] + h_modx_csim[,c(5:9)] %*% cicada
y_pred_2 =  rnorm(n=n_sim, mu_pred2, h_modx_csim[, "sig"])
mu_pred3 =  h_modx_csim[,"a[3]"] + h_modx_csim[,c(5:9)] %*% cicada
y_pred_3 =  rnorm(n=n_sim, mu_pred3, h_modx_csim[, "sig"])

mean(y_pred_1)
mean(y_pred_2)
mean(y_pred_3 > y_pred_2)
mean(y_pred_3 > y_pred_1)
hist(y_pred_1)
hist(y_pred_2)
hist(y_pred_3)

plot(density(y_pred_1),xlab="BL", ylab="density", main="Posterior distribution of BL ")
lines(density(y_pred_2), col="red")
lines(density(y_pred_3), col="blue")
legend("topleft", legend=c("tredecula", "tredecassini", "tredecim "),
       lty=1, col=rep(c("black", "red","blue"), each=1), bty="n")

#gender based pred

cicada_m = c(.20,24,8,2,48)
cicada_f = c(.20,24,8,1,24)
(n_sim = nrow(h_modx_csim))
mu_pred_m1 =  h_modx_csim[,"a[1]"] + h_modx_csim[,c(5:9)] %*% cicada_m
y_pred_m1 =  rnorm(n=n_sim, mu_pred_m1, h_modx_csim[, "sig"])

mu_pred_f1 =  h_modx_csim[,"a[1]"] + h_modx_csim[,c(5:9)] %*% cicada_f
y_pred_f1 =  rnorm(n=n_sim, mu_pred_f1, h_modx_csim[, "sig"])

mu_pred_m2 =  h_modx_csim[,"a[2]"] + h_modx_csim[,c(5:9)] %*% cicada_m
y_pred_m2 =  rnorm(n=n_sim, mu_pred_m2, h_modx_csim[, "sig"])

mu_pred_f2 =  h_modx_csim[,"a[1]"] + h_modx_csim[,c(5:9)] %*% cicada_f
y_pred_f2 =  rnorm(n=n_sim, mu_pred_f2, h_modx_csim[, "sig"])

mu_pred_m3 =  h_modx_csim[,"a[3]"] + h_modx_csim[,c(5:9)] %*% cicada_m
y_pred_m3 =  rnorm(n=n_sim, mu_pred_m3, h_modx_csim[, "sig"])

mu_pred_f3 =  h_modx_csim[,"a[3]"] + h_modx_csim[,c(5:9)] %*% cicada_f
y_pred_f3 =  rnorm(n=n_sim, mu_pred_f3, h_modx_csim[, "sig"])

mean(y_pred_f1 > y_pred_m1)


plot(density(y_pred_m1),xlab="BL", ylab="density", main="Posterior distribution of BL (female vs male) ", lty=2)
lines(density(y_pred_f1), col="black", lty=1)
lines(density(y_pred_m2), col="red" ,lty = 2)
lines(density(y_pred_f2), col="red" ,lty=1)
lines(density(y_pred_m3), col="blue", lty=2)
lines(density(y_pred_f3), col="blue", lty=1)
legend("topleft", legend=c("tredecula M","tredecula F", "tredecassini M","tredecassini F","tredecim M", "tredecim F"),
       lty=c(2,1,2,1,2,1), col=rep(c("black", "red","blue"), each=2), bty="n")

# new species

a_pred = rnorm(n=n_sim,h_mod_csim[,"a0"],h_mod_csim[,"tau"])
X_pred = c(.20,24,8,1,24)
mu_pred = a_pred + h_mod_csim[,c(5:9)] %*% X_pred
y_pred =  rnorm(n=n_sim, mu_pred, h_mod_csim[, "sig"])

head(y_pred)
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
  plot(h_modx_sim , ask=TRUE)

  gelman.diag(h_modx_sim)
  autocorr.diag(h_modx_sim[,"a[1]"],2000)
  autocorr.plot(h_modx_sim)
  effectiveSize(h_modx_sim)
}

#levels: 1:all, beg, exp, inter

dic2 = dic.samples(mod1, n.iter=1e3)


pm_coef = colMeans(mod2_csim)



