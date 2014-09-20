## This program defines two functions: makeCacheMatrix and cacheSolve.

## makeCacheMatrix() will create a list that holds a matrix and its inverse.
## get() and set() are methods responsible for getting and setting the matrix's contents.
## getInv() and setInv() are responsible for getting and setting the intervse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(m) {
        x <<- m
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(v = matrix()) inv <<- v
    getInv <- function() inv
    
    list(set = set, get = get, setInv = setInv, getInv = getInv)

}

## cacheSolve() will return the inverse of the matrix.
## If the inverse is cached, it will use this cached version
## or else, it will compute the inverse.

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("Getting Cached Data")
        return(inv)
    }
    inv <- solve(x$get(), ...)
    x$setInv(inv)
    inv    
}

