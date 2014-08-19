## The makeCacheMatrix and cacheSolve functions create a special object
## that calculates and caches the inverse of a matrix, allowing it to be
## retrievd from the cache when available rather than being recomputed.  

## The makeCacheMatrix function creates a special "matrix" object.
## It calculates and caches the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) i<<- solve
        getsolve <- function() i
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)       

}


## The cacheSolve function first checks to see if the inverse of the
## matrix has already been calculated.  If it has already been calculated
## and the matrix has not changed, then the function retrieves the inverse
## from the cache and skips the computation. Otherwise, it calculates the
## inverse of the matrix and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getsolve()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <-x$get()
        i <- solve(data, ...)
        x$setsolve(i)
        i
}