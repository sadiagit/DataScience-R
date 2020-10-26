lg = function(mu, n, ybar) {
  mu2 = mu^2
  n * (ybar * mu - mu2 / 2.0) - log(1 + mu2)
}
mh = function(n, ybar, n_iter, mu_init, cand_sd) {
  ## Random-Walk Metropolis-Hastings algorithm

  ## step 1, initialize
  mu_out = numeric(n_iter)
  accpt = 0
  mu_now = mu_init
  lg_now = lg(mu=mu_now, n=n, ybar=ybar)

  ## step 2, iterate
  for (i in 1:n_iter) {
    ## step 2a
    mu_cand = rnorm(n=1, mean=mu_now, sd=cand_sd) # draw a candidate

    ## step 2b
    lg_cand = lg(mu=mu_cand, n=n, ybar=ybar) # evaluate log of g with the candidate
    lalpha = lg_cand - lg_now # log of acceptance ratio
    alpha = exp(lalpha)

    ## step 2c
    u = runif(1) # draw a uniform variable which will be less than alpha with probability min(1, alpha)
    if (u < alpha) { # then accept the candidate
      mu_now = mu_cand
      accpt = accpt + 1 # to keep track of acceptance
      lg_now = lg_cand
    }

    ## collect results
    mu_out[i] = mu_now # save this iteration's value of mu
  }

  ## return a list of output
  list(mu=mu_out, accpt=accpt/n_iter)
}

y = c(-0.2, -1.5, -5.3, 0.3, -0.8, -2.2)
ybar = mean(y)
n = length(y)
hist(y, freq=FALSE, xlim=c(-6.0, 3.0)) # histogram of the data
curve(dt(x=x, df=1), lty=2, add=TRUE) # prior for mu
points(y, rep(0,n), pch=1) # individual data points
points(ybar, 0, pch=19) # sample mean

library("coda")

set.seed(61)
post0 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=30, cand_sd=0.5)
post0$accpt
coda::traceplot(as.mcmc(post0$mu))


set.seed(61)
post1 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=18, cand_sd=1.5)
post1$accpt
coda::traceplot(as.mcmc(post1$mu))

set.seed(61)
post2 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=0.0, cand_sd=3.0)
post2$accpt
coda::traceplot(as.mcmc(post2$mu))

set.seed(61)
post3 = mh(n=n, ybar=ybar, n_iter=1e3, mu_init=50, cand_sd=4.0)
post3$accpt
coda::traceplot(as.mcmc(post3$mu))

pmc = mcmc.list(as.mcmc(post0$mu), as.mcmc(post1$mu), as.mcmc(post2$mu),
                as.mcmc(post3$mu))
str(pmc)
coda::traceplot(pmc)

nburn = 200 # remember to discard early iterations
post1$mu_keep = post1$mu[-c(1:nburn)]
summary(as.mcmc(post1$mu_keep))
##

plot(density(post1$mu_keep, adjust=2.0), main="", xlim=c(-6, 3.0), xlab=expression(mu)) # plot density estimate of the posterior
curve(dt(x=x, df=1), lty=2, add=TRUE) # prior for mu
points(ybar, 0, pch=19) # sample mean

curve(5*exp(lg(mu=x, n=n, ybar=ybar)), from=-6, to=3.0, add=TRUE, col="blue") # approximation to the true posterior in blue

