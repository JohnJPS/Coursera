f2 <- function(n) {
dfz <- data.frame(x = numeric(), y = character(), stringsAsFactors = FALSE)
    for(i in 1:n){
        dfz[i,] <- list(i, toString(i))
    }
    dfz
}
