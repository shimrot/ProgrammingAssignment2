## This module defines a cacheable matrix object
##    Use makeCacheMatrix to build the matrix with associated methods
##    Use cacheSolve on the matrix to generate a inverse matrix


## makeCacheMatrix: generate a cacheable matrix
## arguments: 
##    x: regular matrix object (default is 1x1 matrix with NA value)
## returns:
##    cacheable matrix copy of x
makeCacheMatrix <- function(x = matrix()) {

    # copy of x remains in environment, so don't a new variable
    xI <- NULL 
    set <- function(m) {
        x  <<- m  # value of x in parent environment of "set" is replaced
        xI <<- NULL
    }
    get <- function() x
    setInverse <- function(mI) xI <<- mI
    getInverse <- function() xI
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## cacheSolve: solve for inverse of cacheable matrix
## arguments:
##   x: cacheable matrix
##   ...:  optional arguments passed onto R solve function
## returns:
##   inversion matrix
## Assumptions: 
##   * x matrix is invertable
##   * x matrix is square
cacheSolve <- function(x, ...) {
    if (class(x) != "list" || !("setInverse" %in% names(x))) {
        warning("cacheSolve argument x is not a cacheMatrix (decorated matrix) object")
        return(NULL)
    }
    invX <- x$getInverse()
    if(!is.null(invX)) {
        message("getting cached data")
        return(invX)
    }
    
    data <- x$get()
    
    # make sure matrix is not empty or only NA val
    if (nrow(data) > 0 && (nrow(data) > 1 || !is.na(data[1,1]))) {
        # create identity matrix (so optional args don't accidentally replace
        # assumes x is square matrix
        identM <- diag(, nrow(data))   
        
        invX <- solve(data, identM, ...)
        x$setInverse(invX)
    }
    invX
}
