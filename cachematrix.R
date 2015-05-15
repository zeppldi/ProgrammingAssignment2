## The following two functions provide the functionality to cache the
## costly computation of a matrix inverse.

## This function creates a list object out of a matrix containing four
## functions to set and get the matrix and the inverse
## of the matrix

makeCacheMatrix <- function(x = matrix()) {
        cachedInverse <- NULL
        
        set <- function(y){
                x <<- y
                cachedInverse <<- NULL
        }
        
        get <- function() x
        setInverse <- function(inverse) cachedInverse <<- inverse
        getInverse <- function() cachedInverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function checks whether the inverse for the given matrix x (previously created 
## with makeCacheMatrix) is already computed. If so, it returns the computed inverse 
## from the cache. Otherwise it computes the inverse of x with the built-in solve function.

cacheSolve <- function(x, ...) {
        cachedInverse <- x$getInverse()
        
        if (!is.null(cachedInverse)){
                message("getting cached data")
                return(cachedInverse)
        }
        
        matrix <- x$get()
        inverse <- solve(matrix)
        x$setInverse(inverse)
        inverse
}
