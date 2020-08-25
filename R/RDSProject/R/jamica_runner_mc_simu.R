# monte carlo simulation to find the probability of winning all 3 runners from Jamaica
B <- 10000
set.seed(1,kind="rounding")
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

results <- replicate(B,{ #replicate the runner picking func 10000 times
  runners <- sample(runners,3) #out of all runners pick 3
  all(runners == 'Jamaica') #check the runners are all from Jamaica
})

results
print(mean(results))

