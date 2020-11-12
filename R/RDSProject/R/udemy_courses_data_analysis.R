library("rjags")
library("dplyr")

#read file and check data structure
dat = read.csv(file="udemy_courses.csv", header=TRUE)
head(dat)

str(dat)
dim(dat)

# ------------------------- check missing values -------------------#
any(is.na(dat))

#wrangle data
dat$is_paid = as.numeric(dat$is_paid == "True")
dat$level =as.numeric(factor(dat$level))
dat$subject = as.numeric(factor(dat$subject))

# -------------------------- explore data ------------------- #

#check histogram
hist(dat$num_subscribers, breaks=20)
#right skewed positive, so log should be appropriate

min(dat$num_subscribers)
sum(dat$num_subscribers == 0)

plot(jitter(num_subscribers) ~ jitter(price), data=dat, xlab="price", ylab="log(num_subscribers)")

plot(num_subscribers ~ num_reviews, data=dat, xlab="num_reviews", ylab="log(num_subscribers)")
plot(log(dat$num_subscribers + 0.1) ~ dat$is_paid, subset=dat$num_subscribers > 0 )

plot(dat$num_subscribers ~ dat$num_lectures)
boxplot(log(dat$num_subscribers+0.1) ~ dat$level)
plot(dat$num_subscribers ~ dat$content_duration)
plot(log(dat$num_subscribers+0.1) ~ dat$subject)
plot(log(dat$price+0.1) ~ dat$subject)
boxplot(dat$price ~ dat$content_duration)




#------------------------------ create test and train set -----------------------#
train_index <- sample(1:nrow(dat), 0.7 * nrow(dat))
test_index <- setdiff(1:nrow(dat), train_index)
udemy_train_set <- dat[train_index, c(4:10,12)]
udemy_test_set <- dat[test_index, c(4:10,12)]


str(udemy_test_set)
dim(udemy_test_set)
head(udemy_test_set)

#-------------------- Modelling ---------------------#
data_jags = as.list(udemy_train_set)

# model 1:
mod_string_1 = " model {
    for (i in 1:length(num_subscribers)) {
        num_subscribers[i] ~ dpois(lam[i])
        log(lam[i]) = int + b_num_reviews * num_reviews[i] +
                            b_is_paid * is_paid[i] +
                            b_price * price[i] +
                            b_num_lec * num_lectures[i] +
                            b_content_dur * content_duration[i]
    }

    int ~ dnorm(0.0, 1.0/1e6)
    b_num_reviews ~ dnorm(0.0, 1.0/1e4)
    b_is_paid ~ dnorm(0.0, 1.0/1e4)
    b_price ~ dnorm(0.0, 1.0/1e4)
    b_num_lec ~ dnorm(0.0, 1.0/1e4)
    b_content_dur ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

params = c("int", "b_num_reviews", "b_is_paid", "b_price","b_num_lec","b_content_dur")

mod1 = init_model(mod_string_1, data_jags,3)
mod1_sim = simulate_model(mod1,params,1e4)
mod1_csim = combine_simu(mod1_sim)

diagnose_covergence(mod1_sim)
summary(mod1_sim)
dic1 = dic.samples(mod1,n.iter=1e3)


#model 2:

mod_string_2 = " model {
    for (i in 1:length(num_subscribers)) {
        num_subscribers[i] ~ dnorm(mu[i],prec)
        mu[i] = a[subject[i]] + b_num_reviews * num_reviews[i] +
                            b_is_paid * is_paid[i] +
                            b_price * price[i] +
                            b_num_lec * num_lectures[i] +
                            b_content_dur * content_duration[i]
    }

    for (i in 1:max(subject)) {
       a[i] ~ dnorm(a0, prec_a)
    }
    a0 ~ dnorm(0.0, 1.0/1e6)
    prec_a ~ dgamma(1/2.0, 1*1000.0/2.0)
    tau = sqrt( 1.0 / prec_a )

    b_num_reviews ~ dnorm(0.0, 1.0/1e4)
    b_is_paid ~ dnorm(0.0, 1.0/1e4)
    b_price ~ dnorm(0.0, 1.0/1e4)
    b_num_lec ~ dnorm(0.0, 1.0/1e4)
    b_content_dur ~ dnorm(0.0, 1.0/1e4)

     prec ~ dgamma(5/2.0, 5*1000.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

set.seed(102)

params = c("a","tau", "b_num_reviews", "b_is_paid", "b_price","b_num_lec","b_content_dur")

mod2 = init_model(mod_string_2, data_jags,3)

mod2_sim = simulate_model(mod2,params,1e4)
mod2_csim = combine_simu(mod2_sim)

diagnose_covergence(mod2_sim)
summary(mod2_sim)
dic2 = dic.samples(mod2, n.iter = 1e3)


#model 3:
mod_string_3 = " model {
    for (i in 1:length(num_subscribers)) {
        num_subscribers[i] ~ dnorm(mu[i], prec)
       mu[i] = a[subject[i],level[i]] +
                            b_num_reviews * num_reviews[i] +
                            b_is_paid * is_paid[i] +
                            b_price * price[i] +
                            b_num_lec * num_lectures[i] +
                            b_content_dur * content_duration[i]
    }

    for (i in 1:max(subject)) {
      for (j in 1:max(level)) {

       a[i,j] ~ dnorm(a0, prec_a)
      }
    }

    a0 ~ dnorm(0.0, 1.0/1e4)
    prec_a ~ dgamma(1/2.0, 1*1.0/2.0)
    tau = sqrt( 1.0 / prec_a )

    b_num_reviews ~ dnorm(0.0, 1.0/1e4)
    b_is_paid ~ dnorm(0.0, 1.0/1e4)
    b_price ~ dnorm(0.0, 1.0/1e4)
    b_num_lec ~ dnorm(0.0, 1.0/1e4)
    b_content_dur ~ dnorm(0.0, 1.0/1e4)

     prec ~ dgamma(5/2.0, 5*5000.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

set.seed(102)

params = c("a","tau","b_num_reviews", "b_is_paid", "b_price","b_num_lec","b_content_dur")

mod3 = init_model(mod_string_3, data_jags,3)

mod3_sim = simulate_model(mod3,params,1e3)
mod3_csim = combine_simu(mod3_sim)

diagnose_covergence(mod3_sim)
summary(mod3_sim)
dic3 = dic.samples(mod3, n.iter = 1e3)


# model 4:

mod_string_4 = " model {

    for (i in 1:length(num_subscribers)) {
        num_subscribers[i] ~ dnorm(mu[i], prec)
        mu[i] = a[subject[i],level[i]] +
                            b_num_reviews * num_reviews[i] +
                            b_is_paid * is_paid[i] +
                            b_price * price[i] +
                            b_num_lec * num_lectures[i] +
                            b_content_dur * content_duration[i] +
                            b_intx * price[i] * level[i]

    }

    for (i in 1:max(subject)) {
      for (j in 1:max(level)) {

       a[i,j] ~ dnorm(a0, prec_a)

      }
    }

    a0 ~ dnorm(0.0, 1.0/1e4)
    prec_a ~ dgamma(1/2.0, 1*1000.0/2.0)
    tau = sqrt( 1.0 / prec_a )

    b_num_reviews ~ dnorm(0.0, 1.0/1e4)
    b_is_paid ~ dnorm(0.0, 1.0/1e4)
    b_price ~ dnorm(0.0, 1.0/1e4)
    b_num_lec ~ dnorm(0.0, 1.0/1e4)
    b_content_dur ~ dnorm(0.0, 1.0/1e4)
    b_intx ~ dnorm(0.0, 1.0/1e4)

    prec ~ dgamma(5/2.0, 5*5000.0/2.0)
    sig = sqrt( 1.0 / prec )
} "

set.seed(102)

params = c("a","tau","b_intx", "b_num_reviews", "b_is_paid", "b_price","b_num_lec","b_content_dur")

mod4 = init_model(mod_string_4, data_jags,3)

mod4_sim = simulate_model(mod4,params,1e3)
mod4_csim = combine_simu(mod4_sim)

diagnose_covergence(mod4_sim)
summary(mod4_sim)
dic4 = dic.samples(mod4, n.iter = 1e3)

#----------------------------- neg bino--------------------------
mod_string_nb = " model {
    for (i in 1:length(num_subscribers)) {
        num_subscribers[i] ~ dnegbin(p[i], r)
        p[i] = r/(r+lam[i])
        log(lam[i]) = int + b_num_reviews * num_reviews[i] +
                            b_is_paid * is_paid[i] +
                            b_price * price[i] +
                            b_num_lec * num_lectures[i] +
                            b_content_dur * content_duration[i]
    }

    r ~ dgamma(0.01,0.01)
    int ~ dnorm(0.0, 1.0/1e6)
    b_num_reviews ~ dnorm(0.0, 1.0/1e4)
    b_is_paid ~ dnorm(0.0, 1.0/1e4)
    b_price ~ dnorm(0.0, 1.0/1e4)
    b_num_lec ~ dnorm(0.0, 1.0/1e4)
    b_content_dur ~ dnorm(0.0, 1.0/1e4)
} "

set.seed(102)

params = c("int","r","p", "b_num_reviews", "b_is_paid", "b_price","b_num_lec","b_content_dur")

mod_nb = init_model(mod_string_nb, data_jags,3)
mod_nb_sim = simulate_model(mod_nb,params,1e3)
mod_nb_csim = combine_simu(mod_nb_sim)

diagnose_covergence(mod1_sim)
summary(mod1_sim)
dic_nb = dic.samples(mod_nb,n.iter=1e3)
#--------------------------- modelchecking -------------------------------#



pmed_coef = apply(mod3_csim, 2, mean)

udemy_test_set_1_1 <- udemy_train_set %>% filter(level==1 & subject == 1)


X = as.matrix(sapply(udemy_test_set_1_1[,c(1,2,4:8)],as.numeric))


X = cbind(rep(1.0, length(udemy_test_set_1_1$num_subscribers)), X)
head(X)

y_hat = drop(X[,c(1:5,7)] %*% pmed_coef[c("a[1,1]","b_is_paid","b_price",
                                                                "b_num_reviews",
                                                                "b_num_lec",
                                                                "b_content_dur")])



head(y_hat)
head(udemy_test_set_1_1$num_subscribers)


resid = udemy_test_set_1_1$num_subscribers - y_hat
plot(resid) # the data were ordered

plot(y_hat,udemy_test_set_1_1$num_subscribers)

head(udemy_test_set_1_1$num_subscribers)
head(lam_hat)

head(mod4_csim)
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

  gelman.diag(mod3_sim)
  autocorr.diag(mod_nb_sim)
  autocorr.plot(mod3_sim)
}

#levels: 1:all, beg, exp, inter
