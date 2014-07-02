##
## Matrix with chache for the inverse
## 

## function that creates a "CacheMatrix" object
makeCacheMatrix <- function(cm = matrix()) {

    #initialize the inverse to NULL when the CacheMatrix is created
    inv <- NULL
    
    ## create a "Setter" for caching a different matrix
    set <- function(y) {
        cm <<- y
        inv <<- NULL
    }
    
    ## create a Getter to return the cached matrix
    get <- function() { 
        cm
    }
    
    ## create a setter to store the inverse of the matrixe
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    ## create a getter to return the inverse of the matrix
    getInverse <- function() {
        inv
    }
    
    ## return a list of functions for operating on the cached matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## given a CacheMatrix object, "laze solve" for its inverse
## i.e. if the inverse has already been calculated, use the cached inverse
##      else, calculate it and cache the result prior to returning it.
cacheSolve <- function(x, ...) {
    ##check the cache
    inv <- x$getInverse()
    if(!is.null(inv)) {
        ## inverse was cached: return it immediately
        return(inv)
    }
    ## inverse was not cached...
    ## retrieve actualy matrix
    data <- x$get()
    ## calculate the inverse
    inv <- solve(data, ...)
    ## cache the inverse
    x$setInverse(inv)
    ## return the result
    inv
}
