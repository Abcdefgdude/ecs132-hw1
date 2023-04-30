PAMsim <- function(nGen) {
  rows <- nGen + 2
  cols <- nGen + 2
  attachHistory <- vector(length = nGen)
  adjMat <- matrix(data=0, nrow = rows, ncol = cols)
  # nodes 1 and 2 begin connected
  adjMat[1,2] <- 1
  adjMat[2,1] <- 1
  p <- vector(length = rows)
  numConnections <- vector(length = rows)
  numConnections[1] <- 1
  numConnections[2] <- 1
  edges <- 2
  for (i in 1:nGen) {
    # p is the vector of weights for each node
    # calc the weight of each node as the proportion of connections
    # select new node based on above weights
    newNode <- sample(1:rows,1,replace = FALSE, prob = numConnections/edges)
    # connect nodes in adjacency matrix
    numConnections[newNode] <- numConnections[newNode] + 1
    numConnections[i+2] <- numConnections[i+2] + 1
    edges <- edges + 2
    adjMat[i+2,newNode] <- 1 
    adjMat[newNode, i+2] <- 1 
    # record node in history
    attachHistory[i] <- newNode
  }
  return (list(adjMat, attachHistory))
}

PAMemaxd <- function(nGen, nReps) {
  maxdegrees <- vector(length=nReps)
  for (i in 1:nReps) {
    attachHistory <- PAMsim(nGen)[2]
    attachTable <- sort(table(attachHistory),decreasing=TRUE)
    maxdegrees[i] <- attachTable[1]
  }
  return (mean(maxdegrees))
}