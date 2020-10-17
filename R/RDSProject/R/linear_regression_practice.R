# 23 previous space shuttle launches before the Challenger disaster
# T is the temperature in Fahrenheit, I is the O-ring damage index

oring=read.table("http://www.randomservices.org/random/data/Challenger2.txt",header=T)
attach(oring)
plot(T,I)

#linear regression
oring.lm=lm(I~T)
summary(oring.lm)

# add fitted line to scatterplot
lines(T,fitted(oring.lm))

# 95% posterior interval for the slope
-0.24337 - 0.06349*qt(.975,21)
-0.24337 + 0.06349*qt(.975,21)

# the Challenger launch was at 31 degrees Fahrenheit
# how much o-ring damage would we predict?
# y-hat
18.36508-0.24337*31 #(y = b_0+b_1*x)

coef(oring.lm)
coef(oring.lm)[1] + coef(oring.lm)[2]*31

# posterior prediction interval (same as frequentist)
predict(oring.lm,data.frame(T=31),interval="predict")
10.82052-2.102*qt(.975,21)*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))


# posterior probability that damage index is greater than zero
1-pt((0-10.82052)/(2.102*sqrt(1+1/23+((31-mean(T))^2/22/var(T)))),21)
