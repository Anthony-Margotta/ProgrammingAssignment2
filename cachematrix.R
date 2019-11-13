## Functions to save computational time when reusing the inverse of a matrix.
## Matrix inverse is cached upon first calculation and can be retrieved later.

## Creates a special "matrix", a list of functions to manage the solving and 
## caching of a matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solved) inv <<- solved
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Checks to see if the inverse has already been calculated in the special
## matrix above, if so, it retrieves it. Otherwise it calculates it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv  ## Return a matrix that is the inverse of 'x'
}
