tags <- function(m, k, s) {
    sum <- 0
    temp <- 0

    if (temp < s) {
        sum <- sum + sample(1:m, k, replace = TRUE)
        s <- s - 1
        tags(m, sum, s)
    }
}


"
tags(m = number of tags, 
        k = the sum we want to achieve, 
        s = number of tag draws it takes)
        returns the probability it takes s tag draws to achieve
"

print(tags(10, 3, 5))