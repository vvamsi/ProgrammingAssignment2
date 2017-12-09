## There are two functions in this file. The functions 
## are useful when you want to compute the inverse of a matrix
## multiple times. The inverse is computed the first time 
## using the 'solve' function. The computed value is cached
## and subsequent invocations return the cached value.

## makeCacheMatrix takes a single matrix as an argument and returns a new object.
## the returned object supports 4 functions - set, get, setInverse, getInverse
## set, get - allow you to update the matrix, or retrieve it
## setInverse, getInverse - should be used to store the inverse of the original matrix


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list (set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function should be passed on object, x, that has been 
## created by the 'makeCacheMatrix' method. When invoked it will 
## return the inverse of the matrix stored inside x. The inverse is calculated
## the first time and cached within x. Subsequent invocations return the
## cached values

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
