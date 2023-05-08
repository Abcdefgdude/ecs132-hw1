
"
tags(m = number of tags, 
        k = the sum we want to achieve, 
        s = number of tag draws it takes)
        returns the probability it takes s tag draws to achieve
"
tags <- function(m,k,s) {
  # base case
  if (s == 0) {
    if (k <= 0) return (1) # success case
    else return (0) # failure case
  }
  if (m <= 0) return (0) # edge case
  if (k <= 0) return (0) # edge case
  successes <- 0 # amount of success from children calls
  for (i in 1:m) { # call each possible value from 1 to m
    trial <- tags(m,k-i,s-1) # recursive call, returns proportion of successful subchildren
    successes <- (successes + trial) # add up all children in running sum
  }
  return (successes/(m)) # return value divided by m to account for size of subtree
}

print(tags(3,5,2))