
## makeCacheMatrix would create a list of functions which would 
## be used for caching the matrix and its inverse
library(MASS)
makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setInverse <- function(minverse) minv <<- minverse
        getInverse <- function() minv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
}


## cacheSolve would compute the inverse of the matrix and return the 
## cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getInverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- ginv(data, ...)
        x$setInverse(minv)
        minv
}
