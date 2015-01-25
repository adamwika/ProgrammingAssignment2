## makeChacheMatrix gives you functions for setting (globally) and getting your matrix
## and two functions for setting (globally) and getting its inverse.
## cacheSole returns the inverse, if its already set, or calculates a new one with solve(),
## if it's not, sparing you unnecessary calculations

## Defines the set and get functions, returns the list of all four functions

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## If set, returns the global inverse. If not, calls solve on your matrix, and sets inverese.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
