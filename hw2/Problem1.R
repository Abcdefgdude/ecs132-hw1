PAMsim <- function(nGen) {
  rows <- nGen + 2
  cols <- nGen + 2
  attachHistory <- vector(length = nGen)
  adjMat <- matrix(data=0, nrow = rows, ncol = cols)
  # nodes 1 and 2 begin connected
  adjMat[1,2] <- 1
  adjMat[2,1] <- 1
  numConnections <- vector(length = rows)
  numConnections[1] <- 1
  numConnections[2] <- 1
  edges <- 2
  for (i in 1:nGen) {
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
  maxdegrees <- vector(length = nReps)
  for (i in 1:nReps) {
    adjMatrix <- PAMsim(nGen)[[1]]
    connections <- vector(length = nGen + 2)
    for (j in 1:(nGen + 2))
      connections[j] <- sum(adjMatrix[j, ])
    connections <- sort(connections, decreasing = TRUE)
    maxdegrees[i] <- connections[1]
  }
  return (mean(maxdegrees))
}