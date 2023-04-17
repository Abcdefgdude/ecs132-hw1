ExactAnalysis <- function() {
  m <- 5
  p <- c(0.5,0.5)
  v <- 2
  k <- 1
  r <- 3
  q <- 1
  nDays <- 100000
  " k = 1, m = 5, P(bus gets there at t=6) = P(bus leaves the station at 6 -v = 4)
  P(bus leaves at 4) = aP(L_1 = 2 and L_2 = 2) 
  or P(L_1 = 1 and L_2 = 1 and L_3 = 1 and L_4 = 1)
  or P(L_1 = 2 and L_2 = 1 and L_3 = 1)
  or P(L_1 = 1 and L_2 = 2 and L_3 = 1)
  or P(L_1 = 1 and L_2 = 1 and L_# = 2)
  Since all values L are independent, we can multiply for 'and' and add for 'or':
  P(L = j) = p[j]
  P(L_1 = 2 and L_2 = 2) = 0.5 * 0.5
  + P(L_1 = 1 and L_2 = 1 and L_3 = 1 and L_4 = 1) = 0.5 * 0.5 * 0.5 * 0.5
  + P(L_1 = 2 and L_2 = 1 and L_3 = 1) = 0.5 * 0.5 * 0.5
  + P(L_1 = 1 and L_2 = 2 and L_3 = 1) = 0.5 * 0.5 * 0.5
  + P(L_1 = 1 and L_2 = 1 and L_# = 2) = 0.5 * 0.5 * 0.5 
  = 0.5^2 + 3(0.5^3) + 0.5^4 = 0.6875 "
  p_wEqualk <- 0.5^2 + 3*(0.5^3) + 0.5^4
  "P(B_2 leaves at time r = 3) 
   P"
  p_bus2LeavesAtR <- (p[1] * p[2]) + (p[2] * p[1])
  p_WequalKgivenL1eqQ <- 
#  p_Ueq3
#  exp_Wait
#  var_W
#  exp_Bus
#  exp_numBusesByM
#  var_numBusesByM
#  exp_numBusesLeavingGivenWeqK
  print(busSim(m,p,v,k,r,q,nDays))
  return (list(p_wEqualk,p_bus2LeavesAtR, p_WequalKgivenL1eqQ))
}
# in our "notebook," 1 line = 1 day

busSim <- function(m,p,v,k,r,q,nDays)
{
  wVals <- vector(length=nDays)  # set up space for the Ws
  for (day in 1:nDays) {
    wVals[day] <- generateW(v,p,m)
  }
  # T and F treated as 1 and 0; mean of 1s and 0s
  # is proportion of 1s
  return(mean(wVals == k))  
}

generateW <- function(v,p,m) 
{
  tot <- v
  while (1) {
    tot <- tot + generateL(p)
    if (tot >= m) break
  }
  return(tot - m)
}

generateL <- function(p) 
{
  # e.g. if p = (0.2,0.2,0.6), choose 1 number at random from the set
  # 1,2,3, with probabilities 0.2, 0.2 and 0.6, respectively
  sample(1:length(p),1,prob=p)
}
