#suppose we are giving 2 students a multiple choice exam with 40 questions
#where each question has four choices. we dont know how much each student studied but we think they would better than just guessing

#what are the parameters of interest

#theta1= true probability that 1st student will answer the questions correctly
#theta2= true probability that 1st student will answer the questions correctly

#What is the likelihood?
#its a binomial likelihood Bin(40, theta). We assume each question is independent and probability that a student will answer each questions correctly  is equal likely.

#what prior should we use?

#prior is beta conjugate
theta = seq(from=0,to=1, by=0.1)
plot(theta, dbeta(theta,1,1), type='l')
plot(theta, dbeta(theta,4,2), type='l')
plot(theta, dbeta(theta,8,4), type='l')

#what is the prior probability p(theta >0.25)
1-pbeta(0.25,8,4)


#what is the prior probability p(theta >0.5)
1-pbeta(0.5,8,4)


#what is the prior probability p(theta >0.8)
1-pbeta(0.8,8,4)

#suppose that 1st stu gets 33 question right. what is the post dist for theta
#post dist is Beta(8+33,4+40-33) -> Beta(41,11)

#what is the prior probability p(theta >0.25)
1-pbeta(0.25,41,11)


#what is the prior probability p(theta >0.5)
1-pbeta(0.5,41,11)


#what is the prior probability p(theta >0.8)
1-pbeta(0.8,41,11)

#what is the posterior 95% credible interval for theta
qbeta(0.975,41,11)
qbeta(0.025,41,11)

#suppose that 2nd stu gets 24 question right. what is the post dist for theta
#post dist is Beta(8+24,4+40-24) -> Beta(32,20)

#what is the prior probability p(theta >0.25)
1-pbeta(0.25,32,20)


#what is the prior probability p(theta >0.5)
1-pbeta(0.5,32,20)


#what is the prior probability p(theta >0.8)
1-pbeta(0.8,32,20)

#what is the posterior 95% credible interval for theta
qbeta(0.975,32,20)
qbeta(0.025,32,20)

#what is the probability that ist stu has better chance of getting a question right than the 2nd stu
theta1 = rbeta(1000,41,11)
theta2 = rbeta(1000,32,20)
mean(theta1>theta2)



#coin flip
plot()
