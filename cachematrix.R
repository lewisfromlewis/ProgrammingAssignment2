## Put comments here that give an overall description of what your
## functions do

## set matrix, get the value, set the inverse of the matrix and get that inverse.

makeCachematrix <- function(x = matrix(1:4, 2)) { #Needed this to check
        mInverse <- NULL
        setmat <- function(y = matrix()) {
            x <<- y
            mInverse <<- NULL
        }
        getmat <- function() x
        setinverse <- function(solve) mInverse <<- solve
        getinverse <- function() mInverse
        list(set = setmat, get = getmat,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Checks for cached regenerated original matrix and if not present calculates
## the original matrix by reinversion

cachesolve <- function(x, ...) {
    m <- x$getmat()
    if(!is.null(m) && is.matrix(m)) {
        message("getting cached data")
        return(m)
    } else
    newmatrix <- x$getmat()
    m <- solve(newmatrix, ...)
    x$setinverse(m)
    m
}