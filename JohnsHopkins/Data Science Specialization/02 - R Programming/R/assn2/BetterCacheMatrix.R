## Matrix that will lazy-calculate its inverse
CacheMatrix <- function(cm = matrix()) {

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
        if (is.null(inv)) {
            inv <<- solve(cm)
        }
        inv
    }
    
    ## return a list of functions for operating on the cached matrix
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}
