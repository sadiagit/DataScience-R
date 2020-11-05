#log(E(yi)) = log(lamda) =  b0+b1*x1_i+b2*x2_i
b0=1.5
b1 = -0.3
b2 = 1
x1_i = 0.8
x2_i=1.2
log_lam = b0+b1*x1_i+b2*x2_i
lam = exp(log_lam)

#2.If tt is the amount of time that we observe, and \lambdaλ is the rate of events per unit of time, then the expected number of events is t \lambdatλ and the distribution of the number of events in this time interval is Poisson(tλ).
lamda = 1/15 #15 cust per hour
ppois(21,30)

df=read.csv("C:/Dev/DataScience/R_doc/Bayes/callers.csv", header = TRUE)
plot(df$isgroup2,df$calls/df$days_active)
plot(df$isgroup2,df$calls)
plot(df$isgroup2,df$age)
plot(df$age,df$calls/df$days_active)

library("rjags")
mod_string = " model {
    for (i in 1:length(calls)) {
		  calls[i] ~ dpois( days_active[i] * lam[i] )
		  log(lam[i]) = b0 + b[1]*age[i] + b[2]*isgroup2[i]
    }

	  b0 ~ dnorm(0.0, 1.0/1e2)
	  for (i in 1:2){
	    b[i] ~ dnorm(0.0, 1.0/1e2)

    }
	}"
set.seed(102)

data_jags = as.list(df)
params = c("b0", "b")
# inits1 = function() {
#   inits = list("b0"=rnorm(1,0.0,100.0) ,"b"=rnorm(2,0.0,100.0))
# }

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)
update(mod, 1e3)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=5e3)
mod_csim = as.mcmc(do.call(rbind, mod_sim))

## convergence diagnostics
plot(mod_sim)
summary(mod_sim)

head(mod_csim)
mean(mod_csim[,1] >0)

x1 = c(1,29)

loglam1 = mod_csim[,"b0"] + mod_csim[,c(2,1)] %*% x1
lam1 = exp(loglam1)

n_sim = length(lam1)

y1 = rpois(n=n_sim, lambda=lam1*30)


plot(table(factor(y1, levels=0:10))/n_sim, pch=2, ylab="posterior prob.", xlab="calls")

mean(y1 >= 1)

