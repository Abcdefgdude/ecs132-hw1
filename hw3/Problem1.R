dpark <- function(p,d,i) {
  # probability that you park at d +- i = P(before) + P(after)
  # if i > d you couldn't have parked before
  if (i > d) before <- 0
  # you pass d - i - 1 spots before finding one
  else before <- dgeom(d-i-1, p)
  # you've already reached d, now you see i - 1 full spots before finding one empty spot
  after <- dgeom(i-1, p)
  return (before + after)
}

ppark <- function(p,d,i) {
  # what is the probability that there is at least one spot the window (d-i):(d+i)
  # the size of this window is 2i, or d + i if i > d
  i <- floor(i) # take floor of i to account for decimal inputs (you cant park half a spot away)
  if (i > d) min <- d
  else min <- i
  # at least one open spot is the opposite of there being 0 open spots within the window around
  # P(no spots) = (1-p)^(window)
  return (1 - p)^(max + min)
}

qpark <- function(p,d,q) {
  # try all possible options until match is found
  i <- 1
  while (TRUE) {
    percent <- ppark(p,d,i)
    if (percent >= q) break
  }
  return (i)
}

rpark <- function(n,p,d) {
  # generate n spots 
  spots <- rgeom(n,p)
  # return how far way they are from the destination
  return (abs(spots - d))
}