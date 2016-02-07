## This firstly generated a NULL "Matrix" to hold future values
## then populated the matrix with the call values and assigned to the global
## environment using superassignment operator.  The solve function is called on
## the (initially null) matrix to calculate its inverse, which is then set as _m_.

## set matrix, get the value, set the inverse of the matrix and get that inverse.

makeCachematrix <- function(x = matrix()) {
        m <- NULL
        setmat <- function(y) {
            x <<- y
            m <<- NULL
        }
        getmat <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = setmat, get = getmat,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks for cached regenerated original matrix; if present returns it,
## if not present calculates the original matrix by reinversion,
## then returns that, in either case naming the returned matrix _m_ 

cachesolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
