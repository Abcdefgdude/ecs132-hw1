ExactAnalysis <- function() {
  m <- 5
  p <- c(0.5,0.5)
  v <- 2
  k <- 1
  r <- 3
  q <- 1
  nDays <- 10000
  " k = 1, m = 5, P(bus gets there at t=6) = P(bus leaves the station at 6 -v = 4) < 1 wait
  - P(bus leaves at station 5 - v = 3) < 0 wait
  P(bus leaves at 4) = 
  1) P(L_1 = 2 and L_2 = 2) 
  2) or P(L_1 = 1 and L_2 = 1 and L_3 = 1 and L_4 = 1)
  3) or P(L_1 = 2 and L_2 = 1 and L_3 = 1)
  4) or P(L_1 = 1 and L_2 = 2 and L_3 = 1)
  5) or P(L_1 = 1 and L_2 = 1 and L_# = 2)
  In cases 2), 3), and 4), a bus will arrive at time 5, and the passenger will
  not wait at all, so they must be excluded.
  
  Then, since all values L are independent, we can multiply for 'and' and add for 'or':
  P(L = j) = p[j]
  P(L_1 = 2 and L_2 = 2) = 0.5 * 0.5
  + P(L_1 = 1 and L_2 = 1 and L_# = 2) = 0.5 * 0.5 * 0.5 
  = 0.5^2 + 0.5^3 = 0.3675 "
  p_wEqualk <- 0.5^2 + 0.5^3
  "P(B_2 leaves at time r = 3) 
   There are two scenarios where B_2 leaves at r = 3, L_1 = 1 and L_2 = 2,
  or L_1 = 2 and L_2 = 1. "
  p_bus2LeavesAtR <- (p[1] * p[2]) + (p[2] * p[1])
  "P(W = 1 | L_1 = 1) = P(W = 1 and L_1 = 1) / P(L_1)
  Out of the two possible cases where W = 1, where buses have delay (2,2) 
  and (1,1,2) only the later case has L_1 = 1. In this case (1,1,2) the following 
  buses must leave with delays L_2 = 1 and L_3 = 2 for W = 1. 
  So, P(W = 1 and L_1 = 1) / P(L_1) = 0.5^3 / 0.5"
  p_WequalKgivenL1eqQ <- (p[1] * p[1] * p[2]) / p[1]
  "There are two ways for the first bus to arrive to be the third bus, that is 
  the first three buses leave with delays (1,1,1) or (1,1,2). So the probability
  that the passenger boards the third bus is 2 * 0.5^3"
  p_Ueq3 <- p[1]^3

  print(busSim(m,p,v,k,r,q,nDays))
  return (c(p_wEqualk, p_WequalKgivenL1eqQ))
}
# in our "notebook," 1 line = 1 day

busSim <- function(m,p,v,k,r,q,nDays)
{
  wVals <- vector(length=nDays)  # set up space for the Ws
  busCounts <- vector(length = nDays)
  busCountsByM <- vector(length = nDays)
  bus2LeavesByR <- vector(length = nDays)
  for (day in 1:nDays) {
    res <- generateW(v,p,m)
    wVals[day] <- res[1]
    busCounts[day] <- res[2]
    busCountsByM[day] <- generateBusByM(p,m)
    bus2LeavesByR[day] <- doesBusXLeaveAtTimeR(p,2,r)
  }
  # T and F treated as 1 and 0; mean of 1s and 0s
  # is proportion of 1s
  p_wEqualk <- mean(wVals == k)
  p_bus2LeavesAtR <- mean(bus2LeavesByR)
  p_Ueq3 <- mean(busCounts == 3)
  exp_Wait <- mean(wVals)
  var_W <- mean(wVals^2) - (mean(wVals))^2
    for (day in 1:nDays) wVals[day] <- generateW(v,p,m-1)[1]
  p_WequalKgivenL1eqQ <- mean(wVals == k)
  # E(buses) = long running average
  exp_Bus <- mean(busCounts)
  exp_numBusesByM <- mean(busCountsByM)
  # Var(buses) = E(buses^2) - (E(buses))^2
  var_numBusesByM <- mean(busCountsByM^2) - (mean(busCountsByM))^2
  for (day in 1:nDays) busCounts[day] <- generateW(v,p,m,k)[2]
  exp_numBusesLeavingGivenWeqK <- mean(busCounts)
  return (c(p_wEqualk,p_bus2LeavesAtR, p_WequalKgivenL1eqQ,p_Ueq3,exp_Wait, var_W, 
               exp_Bus, exp_numBusesByM, var_numBusesByM, exp_numBusesLeavingGivenWeqK))
}

"Find the number of buses leaving the main station by time m"
doesBusXLeaveAtTimeR <- function(p,x,r)
{
  buses <- 0 # count of buses
  total <- 0 # how much total delay has there been
  while (1) {
    buses <- buses + 1
    total <- total + generateL(p)
    if (total>r) return (FALSE) # total went past r, return false
    if (total==r) return (buses == x) # total is at r, check number of buses
  }
}

generateBusByM <- function(p,m)
{
  buses <- 0 # count of buses
  total <- 0 # how much total delay has there been
  while (1) {
    buses <- buses + 1 # another bus left
    total <- total + generateL(p)
    if (total>m) return(buses-1) # if total goes above m, take away the last bus
    if (total==m) return(buses) # stop counting at time m 
  }
}

generateW <- function(v,p,m,k=-1) 
{
  buses <- 0 # count of buses
  tot <- v # how much total delay has there been
  while (1) {
    buses <- buses + 1 # another bus left
    tot <- tot + generateL(p)
    if (tot >= m) break # stop counting once bus has arrived at the stop
  }
  # if a given k value was passed, only accept when the wait = k 
  if (k != -1 & tot - m != k) return (generateW(v,p,m,k)) 
  return(c(tot-m,buses))
}

generateL <- function(p) 
{
  # e.g. if p = (0.2,0.2,0.6), choose 1 number at random from the set
  # 1,2,3, with probabilities 0.2, 0.2 and 0.6, respectively
  sample(1:length(p),1,prob=p)
}
