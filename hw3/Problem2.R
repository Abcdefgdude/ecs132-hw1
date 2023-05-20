# pi1 function from book
findpi1 <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1,n)
  rhs <- c(rep(0,n-1),1)
  solve(imp,rhs)
}

boardGame <- function() {
  l <- 8 # number of squares
  b <- 4 # bonus square
  p <- matrix(data=0,nrow=8,ncol=8)
  # generate p by simulating possible rolls
  for (i in 1:l) { # start from any state
    for (j in 1:6) { # roll die from 1:6
      if ((i + j) == l) square <- l # modulo edge case
      else square <- (i + j) %% l
      if (square == b)  # you are on bonus square, add probs to next squares
          for (k in 1:6) {
            if ((square + k) == l) square2 <- l
            else square2 <- (square + k) %% l
          # add 1/6^2 since you have 1/6 chance to land on bonus, times 1/6 to get the k from bonus 
            p[i,square2] <- p[i,square2] + (1/6)^2 
          }
      else {
        # 1/6 chance to land on this square
        p[i,square] <- p[i,square] + 1/6
      }
    }
    print(p)
    pi <- findpi1(p)
    # Expected value =
    # sum for i in 1:8 -> P(square = i) * (P(win|square=i) + P(2win|square=i))
    # P(win|square=i) = probability goes to state > 8
    # P(2win|square=i) = probability goes to state = 4 and goes to state > 8
    EW <- 0 # expected winnings running sum
    winnings <- vector(length=l)
    for (i in 1:l) { # loop over each square
      if ((i + 6) == l) highest <- l
      else highest <- (i+6) %% l
      # check if you can possibly win (not possible from 1 or 2)
      print(highest)
      if (highest < i) {
        # eg if i = 6, highest = 6 + 6 %% 8 = 4. 4 out of 6 possbile rolls are winning
        winnings[i] <- highest/6 
      }
      # 4 is reachable from every square but 4 and 5
      if (i != 4 & i != 5) {
        winnings[i] <- winnings[i] + (2/6) * 1/6 # you have 2/6 chance to win from 4, and 1/6 chance to land on 4
      }
    }
    EW <-  mean(winnings)
    print(winnings %*% pi)
    
  }

}
boardGame()