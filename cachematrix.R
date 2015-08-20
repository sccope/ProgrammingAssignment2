## The following functions cache the inverse of a matrix

## This function creates a special "matrix" object that can 
##cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function computes the inverse of the special "matrix" 
##returned by  makeCacheMatrix  above. If the inverse has already
##been calculated (and the matrix has not changed), 
##then  cacheSolve  should retrieve the inverse from the cache.

##Computing the inverse of a square matrix can be done with the 
##solve  function in R. For example, if  X  is a square invertible 
##matrix, then  solve(X)  returns its inverse.


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
    }
