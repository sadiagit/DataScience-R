library("corrplot")
library("dplyr")
library("rjags")

# Pima Indians Diabetes Database
data(PimaIndiansDiabetes)
dim(PimaIndiansDiabetes)
levels(PimaIndiansDiabetes$diabetes)
head(PimaIndiansDiabetes)
dim(dat)

dat = na.omit(PimaIndiansDiabetes)
dat$diabetes = as.integer(as.factor(dat$diabetes) == 'pos')


Cor = cor(dat)
corrplot(Cor, type="upper", method="ellipse", tl.pos="d")
corrplot(Cor, type="lower", method="number", col="black",
         add=TRUE, diag=FALSE, tl.pos="n", cl.pos="n")


X = scale(dat[c(1:8)], center=TRUE, scale=TRUE)
head(X)
colMeans(X)
apply(X, 2, sd)


mod1_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int +
                  b[1]*pregnant[i] +
                  b[2]*glucose[i] +
                  b[3]*pressure[i] +
                  b[4]*triceps[i] +
                  b[5]*insulin[i] +
                  b[6]*mass[i] +
                  b[7]*pedigree[i] +
                  b[8]*age[i]

    }
    int ~ dnorm(0.0, 1.0/25)
    for (j in 1:8) {
        b[j] ~ ddexp(0.0, sqrt(2.0)) # has variance 1.0
    }


} "


set.seed(92)


data_jags = list(y=dat$diabetes,
                      pregnant=X[,"pregnant"],
                      glucose=X[,"glucose"],
                      pressure=X[,"pressure"],
                      triceps=X[,"triceps"],
                      insulin=X[,"insulin"],
                      mass= X[,"mass"],
                      pedigree = X[,"pedigree"],
                      age= X[,"age"])

params = c("int", "b")

mod1 = init_model(mod1_string, data_jags,3)
mod1_sim = simulate_model(mod1,params,5e3)
mod1_csim = combine_simu(mod1_sim)

plot(mod1_sim, ask=TRUE)

pairs(X[, c(1:9)])

#model 2:
mod2_string = " model {
    for (i in 1:length(y)) {
        y[i] ~ dbern(p[i])
        logit(p[i]) = int +
                  b[1]*pregnant[i] +
                  b[2]*glucose[i] +
                  b[3]*pressure[i] +
                  b[5]*insulin[i] +
                  b[6]*mass[i] +
                  b[7]*pedigree[i] +
                  b[8]*age[i]

    }
    int ~ dnorm(0.0, 1.0/25)
    for (j in 1:8) {
        b[j] ~ dnorm(0.0, 1/1e5)
    }


} "


set.seed(92)


data_jags = list(y=dat$diabetes,
                 pregnant=X[,"pregnant"],
                 glucose=X[,"glucose"],
                 pressure=X[,"pressure"],
                 triceps=X[,"triceps"],
                 insulin=X[,"insulin"],
                 mass= X[,"mass"],
                 pedigree = X[,"pedigree"],
                 age= X[,"age"])

params = c("int", "b")

mod2 = init_model(mod2_string, data_jags,3)
mod2_sim = simulate_model(mod2,params,1e3)
mod2_csim = combine_simu(mod2_sim)

dic2 = dic.samples(mod2, n.iter=1e3)

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

  gelman.diag(mod2_sim)
  autocorr.diag(mod3_sim)
  autocorr.plot(mod2_sim)
}

#levels: 1:all, beg, exp, inter

dic2 = dic.samples(mod2, n.iter=1e3)
dic3 = dic.samples(mod3, n.iter=1e3)

pm_coef = colMeans(mod2_csim)

pm_Xb = pm_coef["int"] +  X %*% pm_coef[1:8]
phat = 1.0 / (1.0 + exp(-pm_Xb))
head(phat)

plot(phat, jitter(data_jags$y))

(tab0.5 = table(phat>0.4, data_jags$y))

sum(diag(tab0.5)) / sum(tab0.5)

summary(mod2_sim)
Glass %>% group_by(Type) %>% summarise(n=n())
