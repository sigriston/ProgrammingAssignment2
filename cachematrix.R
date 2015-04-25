##
## R Programming Course: https://class.coursera.org/rprog-013
## ----------------------------------------------------------
## Assignment 2:
##     Special Matrix object with cached inverse computation
##

##
## makeCacheMatrix - constructs a matrix object with inverse cache.
##
## Arguments:
##     x - initializer matrix
##
## Returns:
##     Special matrix object with a cache for the inverse matrix.
##     See cacheSolve() below for obtaining the inverse matrix from
##     it.
##
makeCacheMatrix <- function(x = matrix()) {
    ## cache for the inverse
    inverse <- NULL
    
    ## getter and setter functions
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    ## getter and setter for the inverse
    getinverse <- function() inverse
    setinverse <- function(inv) inverse <<- inv
    
    ## returned object
    list(get = get, set = set,
         getinverse = getinverse,
         setinverse = setinverse)
}


##
## cacheSolve - returns the inverse of a matrix created with
##              makeCacheMatrix().
##              This function is efficient - it will only compute
##              the inverse matrix if it hasn't been computed before;
##              otherwise it will return a cached result.
##
## Arguments:
##     x   - matrix created with makeCacheMatrix()
##     ... - further arguments get passed to solve()
##
## Returns:
##     Inverse matrix (cached if possible).
##
cacheSolve <- function(x, ...) {
    ## cached inverse
    inv <- x$getinverse()

    ## return cached inverse if exists...
    if (!is.null(inv)) {
        return(inv)
    }
    
    ## ...otherwise, compute the inverse
    mat <- x$get()
    inv <- solve(mat, ...)
    
    ## before returning, store the NEW result in the cache!
    x$setinverse(inv)
    inv
}
