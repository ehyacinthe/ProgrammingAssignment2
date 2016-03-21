## Calculating and Caching the Inverse of a Matrix:
## Below are two functions that are used to create a special object that
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

cachematrix <- function(x=matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function()
         x
        setInverse <- function(solve)
        inverse <<- solve
        getInverse <- function()
         inverse
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by
## cachematrix above. If the inverse has already been calculated (and the
## matrix has not changed), then it should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        mat <- x$get()
        inverse <- solve(mat, ...)
        x$setInverse(inverse)
        inverse
}
