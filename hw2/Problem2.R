exactEW <- function() {
  # there is a 1/3 probability that the prize is behind the first door, 
  # otherwise 
  return (20000 *1/3 + 200 * 2/3)
}

simEW <- function(nreps) {
  values <- vector(length=nreps) 
  for (i in 1:nreps) {
    prize <- sample(1:3, 1)
    if (prize == 1) values[i] <- 20000
    else values[i] <- 200
  }
  return (mean(values))
}