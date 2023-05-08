tags <- function(m,k,s) {
  if (s == 0) {
    if (k <= 0) {
    #print(c("success:", m, k, s))
    return (1)
    }
    else return (0)
  }
  if (m <= 0) return (0)
  if (k <= 0) return (0)
  successes <- 0
  for (i in 1:m) {
    trial <- tags(m,k-i,s-1)
    successes <- (successes + trial)
  }
  return (successes/(m))
}

"
tags(m = number of tags, 
        k = the sum we want to achieve, 
        s = number of tag draws it takes)
        returns the probability it takes s tag draws to achieve
"
print(tags(3,5,2))