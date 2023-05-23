# pi1 function from book
findpi1 <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  solve(imp,rhs)
}

callCtr <- function(p, q, r, w) {
  # Transition probability matrix
  num_states <- r + w
  P <- matrix(0, nrow = num_states, ncol = num_states)
  
  # Set up transition probabilities for each state
  for (i in 1:num_states) { # i represents the state where we have i calls active + on hold
    if (i == 1) { 
      # No calls in the system
      P[i,i] = 1 - q # a new call does not arrive
      P[i,2] = q # a new call does arrive
    } else if (i <= r) {
      for (j in 0:i)  {# how many existing calls finish
        P[i,i-j+1] <- P[i,i-j+1] + dbinom(j,i,p)*(q) # j calls finish, a new call comes in
        P[i,i-j] <- P[i,i-j] + dbinom(j,i,p)*(1-q) # j calls finish, no new call comes in
      }
    } else if (i < r + w) {
      # Calls in the waiting area
      # a maximum of r calls could finish
      for (j in 0:r) {
        P[i,i-j+1] <- P[i,i-j+1] + dbinom(j,r,p)*(q) # j calls finish, a new call comes in
        P[i,i-j] <- P[i,i-j] + dbinom(j,r,p)*(1-q) # j calls finish, no new call comes in
      }
    } else {
      # Maximum number of calls reached
      # a maximum of r calls could finish
      # if no calls finish, we will always stay in the max call state (whether new call comes in or not)
      P[i,i] <- dbinom(0,r,p)
      for (j in 1:r) {
        P[i,i-j+1] <- P[i,i-j+1] + dbinom(j,r,p)*(q) # j calls finish, a new call comes in
        P[i,i-j] <- P[i,i-j] + dbinom(j,r,p)*(1-q) # j calls finish, no new call comes in
      }
    }
  }
  # Calculate long-run proportions and averages
  pi <- findpi1(P)
  # probability that a call is dropped = 
  # P(full calls) * P(no calls dropped) * P(new call)
  call_dropped <- pi[r+w] * dbinom(0,r,p) * q
  # average number of calls on hold = sum(1,w) of P(holds = i) * i
  average_holds <- (1:w) %*% pi[(r+1):(r+w)]
  #return(results)
  # average number of busy operators = sum(1,r) of P(calls = i) * i + sum(r,r+w) P(holds = i) * i
  average_calls <- (1:r) %*% pi[1:r] + rep(r,w) %*% pi[(r+1):(r+w)]
  return(c(call_dropped,average_holds,average_calls))
}
results <- callCtr(0.5, 0.5, 5, 3)
print(results)
