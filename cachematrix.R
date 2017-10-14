## The program aims to return inverse of a matrix. 
## If its been computed it will be retrieved from cache.

## makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x<<-y
        inv<<-NULL
    }
    get <- function() x
    setinv <- function(inverse) inv<<-inverse
    getinv <- function() inv
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## cacheSolve function computes the inverse of the matrix returned by makeCacheMatrix. 
## If the inverse has already been computed then retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached inv")
        return(inv)
    }
    info <- x$get()
    library(MASS)
    inv <- ginv(info, ...)
    x$setinv(inv)
    inv
}
