## Create vector that "lazy calculates" its mean
CacheVector <- function(v = numeric()) {
    ## set the cacheMean to NULL for starters
    m <- NULL
    ## create a setter that stores v and initializes m
    set <- function(y) {
        v <<- y
        m <<- NULL
    }
    ## create a getter that returns the cached v
    get <- function() {
        v
    }
    ## create a setMean function (would anyone ever use this?)
    setMean <- function(mean) {
        m <<- mean
    }
    ## create a getMean function
    getMean <- function() {
        if (is.null(m)) {
            m <- mean(v)
        }
        m
    }
    ## return the CacheVector object as a list of 4 functions
    list(set = set, get = get, setMean = setMean, getMean = getMean)
}
