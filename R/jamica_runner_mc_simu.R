B <- 10000
set.seed(1,kind="rounding")
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
results <- replicate(B,{
  jamica_runners <- sample(runners,3)
  all(jamica_runners == 'Jamaica')
})
results
print(mean(results))

