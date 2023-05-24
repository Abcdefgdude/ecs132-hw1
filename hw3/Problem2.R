boardGame <- function() {
  p <- matrix(rep(0, 64), nrow = 8) # - 8 total states, so 8x8 matrix with 0's
  for (i in 1:8) {                  # - "Starting from square i..."
    for (j in 1:6) {                # - "...what is the probability we land on j?" 6 possible rolls
      k <- i + j                    
      if (k > 8) k <- k - 8         # - If k < 8, wrap back to the beginning
      if (k == 4) p[i,k] <- 0       # - If k == 4, we hit a bonus, set to 0 since we can't end here
      else p[i,k] <- 1/6            # - There is a 1/6 probability of rolling k from square i
    }
    if (i != 4 & i != 5) {          # - If i == 4 | 5, we can't hit the bonus 
      for (m in 1:6) {              # - Otherwise, we add 1/36 to each square that can be rolled
                                    #   within square 4 (1/6 chance to roll bonus, then 1/6 chance
                                    #   to roll i from square 4: 1/6 * 1/6 = 1/36)
        m <- 4 + m
        if (m > 8) m <- m - 8       # If past 8 wrap back to square 1
        p[i,m] <- p[i,m] + 1/36     
      }
    }
  }
  pi <- findpi(p)
  expectedW <- mean(calculateW(p))
  varW <- mean(calculateW(p)^2) - (mean(calculateW(p)))^2
  return(c(pi, expectedW, varW))
}

calculateW <- function(p) {
  # Expected value =
  # sum for i in 1:8 -> P(square = i) * (P(win|square=i) + P(2win|square=i))
  # P(win|square=i) = probability goes to state > 8
  # P(2win|square=i) = probability goes to state = 4 and goes to state > 8
  
  n <- nrow(p)
  winnings <- vector(length = n)
  EW <- 0
  for (i in 1:n) {
    if (i + 6 == n) 
      highest <- n
    else 
      highest <- (i + 6) %% n                   # check if you can possibly win (not possible from 1 or 2)
    if (highest < i)                            # eg if i = 6, highest = 6 + 6 %% 8 = 4. 4 out of 6 possible rolls are winning
      winnings[i] <- highest / 6
    if (i != 4 & i != 5)                        # 4 is reachable from every square but 4 and 5
      winnings[i] <- winnings[i] + (2/6) * 1/6  # you have 2/6 chance to win from 4, and 1/6 chance to land on 4
  }
  return(winnings)
}

# Findpi function from the book
findpi <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0, n-1), 1)
  solve(imp, rhs)
}
