simEVarW <- function(d, v, p, nreps) {
  prizes <- vector(length = nreps)
  doors <- (1:d)
  for (i in 1:nreps) {
    A <- sample(doors, length(v))
    # host opens a door with no prize
    emptyDoors <- doors[-A]
    H <- sample(emptyDoors[-1], 1)
    C <- 1
    # she switches, pick a new door that's not the one the host picked and is not the first door
    if (runif(1) <= p) {
      C <- sample(doors[-c(1, H)], 1)
    }
    # add prize behind door C to prizes array
    if (C %in% A)
      prizes[i] <- v[which(A == C)]
    else
      prizes[i] <- 0
  }
  EW <- mean(prizes)
  varW <- mean(prizes ^ 2) - (mean(prizes)) ^ 2
  print(EW)
  print(varW)
  return(c(EW, varW))
}

simEVarW(8, c(2, 7, 3, 8), 0.88, 10000)