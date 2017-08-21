## R-Programming
## Programming Assignment 2

## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. Your assignment is to write a pair of functions that cache 
## the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    ## set function overwrites x and clears m
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get function retrieves maxtrix x
    get <- function() x
    ## setinv function writes matrix solve into cache m
    setinv <- function(solve) m <<- solve
    ## getinv function retrieves inverted matrix m
    getinv <- function() m
    ## last expression of function is returned
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    ## if m is not null, return the cached inverse matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## otherwise get the matrix x and assign it to data
    data <- x$get()
    ## calculate the inverse matrix of data and assign it to m
    m <- solve(data, ...)
    ## store the inverse matrix in the cache
    x$setinv(m)
    ## return the inverse matrix result
    m
}